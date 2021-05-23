import java.nio.file.Paths
import org.objectweb.asm.{ClassReader, Type, ClassVisitor}
import org.objectweb.asm.commons.{ClassRemapper, SimpleRemapper}
import org.objectweb.asm.tree.{ClassNode, FieldNode, MethodNode}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import scala.jdk.CollectionConverters.*
import io.bullet.borer.{Cbor, Encoder}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    args match {
      case List("deobfuscate", unobfuscatedJar, obfuscatedJar, outputJar) =>
        val unobfuscatedPath = Paths.get(unobfuscatedJar)
        val obfuscatedPath = Paths.get(obfuscatedJar)
        val outputPath = Paths.get(outputJar)
        for
          obfuscated <- Jar(obfuscatedPath).use(JarSignature.fromJar)
          unobfuscated <- Jar(unobfuscatedPath).use(JarSignature.fromJar)
          matches = JarSignature.findMatches(obfuscated, unobfuscated)
          _ = println("Writing the deobfuscated file...")
          _ <- Jar(obfuscatedPath).use {
            _.transform(outputPath)(Deobfuscator(matches)).compile.drain
          }
        yield ExitCode.Success

      case List("save", unobfuscatedJar, obfuscatedJar, output) =>
        val unobfuscatedPath = Paths.get(unobfuscatedJar)
        val obfuscatedPath = Paths.get(obfuscatedJar)
        val outputPath = Paths.get(output)
        for
          obfuscated <- Jar(obfuscatedPath).use(JarSignature.fromJar)
          unobfuscated <- Jar(unobfuscatedPath).use(JarSignature.fromJar)
          matches = JarSignature.findMatches(obfuscated, unobfuscated)
          file = Cbor.encode(matches).to(outputPath.toFile).result
          _ = println(s"Output saved to ${file.getPath}")
        yield ExitCode.Success

      case _ =>
        println(
          "Usage: java -jar deobfuscator.jar [deobfuscate|save] [unobfuscated-jar] [obfuscated-jar] [output-path]"
        )
        IO.pure(ExitCode.Success)
    }
  }

final case class JarSignature(signatures: Map[String, List[ClassSignature]])

object JarSignature {
  def fromJar(jar: Jar): IO[JarSignature] =
    jar.classes.compile
      .fold(Map.empty[String, List[ClassSignature]]) { (acc, clazz) =>
        val node = ClassNode()
        clazz.accept(node, ClassReader.SKIP_DEBUG)
        val signature = ClassSignature.fromNode(node)
        acc |+| Map(signature.toString -> List(signature))
      }
      .map(JarSignature.apply)

  def findMatches(
      obfuscated: JarSignature,
      unobfuscated: JarSignature
  ): Map[String, ClassMatch] =
    obfuscated.signatures
      .foldLeft(Map.empty[String, ClassMatch]) { case (acc, (needleSig, needles)) =>
        unobfuscated.signatures.get(needleSig) match
          case Some(res :: Nil) =>
            acc.updated(needles.head.name, ClassMatch(needles.head, res))

          case Some(results) =>
            val needleSigs = needles.groupBy(_.getBodySignature)
            val resultSigs = results.groupBy(_.getBodySignature)

            needleSigs.foldLeft(acc) {
              case (acc, (k, h :: Nil)) =>
                resultSigs.get(k) match
                  case Some(res :: Nil) => acc.updated(h.name, ClassMatch(h, res))
                  case _                => acc
              case _ => acc
            }
          case None => acc
        }
}

final case class ClassSignature(
    name: String,
    fields: List[FieldSignature],
    methods: List[MethodSignature]
) {
  override def toString =
    s"c${fields.mkString(":")}${methods.mkString(":")}}"

  def getBodySignature: String =
    this.methods
      .map { m =>
        s"${m.maxLocals}:${m.maxStack}:${m.instructionCount}"
      }
      .mkString(":")
}

object ClassSignature {
  implicit val encoder: Encoder[ClassSignature] = Encoder { (writer, sig) =>
    writer
      .writeArrayOpen(3)
      .writeString(sig.name)
      .write(sig.fields)
      .write(sig.methods)
      .writeArrayClose()
  }

  def fromNode(node: ClassNode): ClassSignature = {
    val fields = node.fields.asScala.map(FieldSignature.fromNode).toList
    val methods = node.methods.asScala.map(MethodSignature.fromNode).toList
    ClassSignature(node.name, fields, methods)
  }
}

final case class FieldSignature(name: String, descriptor: String, access: Int) {
  override def toString =
    s"f$access${Signature.formatType(Type.getType(descriptor))}"
}

object FieldSignature {
  implicit val encoder: Encoder[FieldSignature] = Encoder { (writer, sig) =>
    writer
      .writeArrayOpen(3)
      .writeString(sig.name)
      .writeString(sig.descriptor)
      .writeInt(sig.access)
      .writeArrayClose()
  }

  def fromNode(node: FieldNode): FieldSignature =
    FieldSignature(node.name, node.desc, node.access)
}

final case class MethodSignature(
    name: String,
    descriptor: String,
    access: Int,
    maxLocals: Int,
    maxStack: Int,
    instructionCount: Int
) {
  override def toString = {
    val typ = Type.getMethodType(descriptor)
    val args = typ.getArgumentTypes
    val ret = typ.getReturnType
    s"m$access(${args.map(Signature.formatType).mkString})${Signature.formatType(ret)}"
  }
}

object MethodSignature {
  implicit val encoder: Encoder[MethodSignature] = Encoder { (writer, sig) =>
    writer
      .writeArrayOpen(6)
      .writeString(sig.name)
      .writeString(sig.descriptor)
      .writeInt(sig.access)
      .writeInt(sig.maxLocals)
      .writeInt(sig.maxStack)
      .writeInt(sig.instructionCount)
      .writeArrayClose()
  }

  def fromNode(node: MethodNode): MethodSignature =
    MethodSignature(
      node.name,
      node.desc,
      node.access,
      node.maxLocals,
      node.maxStack,
      node.instructions.size
    )
}

object Signature {
  def formatType(typ: Type): String =
    typ.getSort match
      case Type.OBJECT
          if typ.getDescriptor().startsWith("Ljava/lang/") ||
            typ.getDescriptor().startsWith("Lorg/apache/") ||
            typ.getDescriptor().startsWith("Lgnu/trove/") =>
        typ.getDescriptor()
      case Type.OBJECT => "LGap;"
      case _           => typ.getDescriptor()
}

final case class ClassMatch(obfuscated: ClassSignature, unobfuscated: ClassSignature)

object ClassMatch {
  implicit val encoder: Encoder[ClassMatch] = Encoder { (writer, clazz) =>
    writer
      .writeArrayOpen(2)
      .write(clazz.obfuscated)
      .write(clazz.unobfuscated)
      .writeArrayClose()
  }
}

class Deobfuscator(mappings: Map[String, ClassMatch]) extends ClassTransformer {
  val remapper = new SimpleRemapper(mappings.view.mapValues(_.unobfuscated.name).toMap.asJava) {
    override def mapFieldName(owner: String, name: String, descriptor: String): String =
      mappings.get(owner)
        .flatMap { classes =>
          val index =
            classes.obfuscated.fields.indexWhere(f => f.name == name && f.descriptor == descriptor)
          if (index >= 0) Some(classes.unobfuscated.fields(index).name) else None
        }
        .getOrElse(name)

    override def mapMethodName(owner: String, name: String, descriptor: String): String =
      mappings.get(owner)
        .flatMap { classes =>
          val index =
            classes.obfuscated.methods.indexWhere(m => m.name == name && m.descriptor == descriptor)
          if (index >= 0) Some(classes.unobfuscated.methods(index).name) else None
        }
        .getOrElse(name)
  }

  override def visitor(visitor: ClassVisitor): ClassVisitor =
    ClassRemapper(visitor, remapper)

  override def name(previous: String): String =
    mappings.get(previous).map(_.unobfuscated.name).getOrElse(previous)
}
