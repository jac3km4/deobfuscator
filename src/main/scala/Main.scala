import java.nio.file.Paths
import org.objectweb.asm.{ClassReader, ClassVisitor, Type}
import org.objectweb.asm.commons.{ClassRemapper, SimpleRemapper}
import org.objectweb.asm.tree.{ClassNode, FieldNode, MethodNode}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import scala.jdk.CollectionConverters.*

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    if (args.length == 3) {
      val unobfuscatedPath = Paths.get(args(0))
      val obfuscatedPath = Paths.get(args(1))
      val outputPath = Paths.get(args(2))
      println("Searching for mappings...")

      for
        obfuscated <- Jar(obfuscatedPath).use(JarSignature.fromJar)
        unobfuscated <- Jar(unobfuscatedPath).use(JarSignature.fromJar)
        matches = JarSignature.findMatches(obfuscated, unobfuscated)
        _ = println("Writing the deobfuscated file...")
        _ <- Jar(obfuscatedPath).use {
          _.transform(outputPath)(Deobfuscator(matches)).compile.drain
        }
      yield ExitCode.Success
    } else {
      println("Usage: java -jar deobfuscator.jar [unobfuscated-jar] [obfuscated-jar] [output-jar]")
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
        s"${m.node.maxLocals}:${m.node.maxStack}:${m.node.instructions.size}"
      }
      .mkString(":")
}

object ClassSignature {
  def fromNode(node: ClassNode): ClassSignature = {
    val fields = node.fields.asScala.map(FieldSignature.apply).toList
    val methods = node.methods.asScala.map(MethodSignature.apply).toList
    ClassSignature(node.name, fields, methods)
  }
}

final case class FieldSignature(node: FieldNode) {
  override def toString =
    s"f${node.access}${Signature.formatType(Type.getType(node.desc))}"
}

final case class MethodSignature(node: MethodNode) {
  override def toString = {
    val typ = Type.getMethodType(node.desc)
    val args = typ.getArgumentTypes
    val ret = typ.getReturnType
    s"m${node.access}(${args.map(Signature.formatType).mkString})${Signature.formatType(ret)}"
  }
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

class Deobfuscator(mappings: Map[String, ClassMatch]) extends ClassTransformer {
  val remapper = new SimpleRemapper(mappings.view.mapValues(_.unobfuscated.name).toMap.asJava) {
    override def mapFieldName(owner: String, name: String, descriptor: String): String =
      mappings
        .get(owner)
        .flatMap { classes =>
          val index = classes.obfuscated.fields.indexWhere(f =>
            f.node.name == name && f.node.desc == descriptor
          )
          if (index >= 0) Some(classes.unobfuscated.fields(index).node.name) else None
        }
        .getOrElse(name)

    override def mapMethodName(owner: String, name: String, descriptor: String): String =
      mappings
        .get(owner)
        .flatMap { classes =>
          val index = classes.obfuscated.methods.indexWhere(m =>
            m.node.name == name && m.node.desc == descriptor
          )
          if (index >= 0) Some(classes.unobfuscated.methods(index).node.name) else None
        }
        .getOrElse(name)
  }

  override def visitor(visitor: ClassVisitor): ClassVisitor =
    ClassRemapper(visitor, remapper)

  override def name(previous: String): String =
    mappings.get(previous).map(_.unobfuscated.name).getOrElse(previous)
}
