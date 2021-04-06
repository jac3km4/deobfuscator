import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}
import cats.effect.{IO, Resource}
import fs2.Stream
import scala.jdk.CollectionConverters.*

class Jar private (zip: ZipFile) {
  def classes: Stream[IO, ClassReader] =
    Stream
      .fromIterator[IO](zip.entries().asScala, 1)
      .filter(_.getName.endsWith(".class"))
      .evalMap { entry =>
        Resource
          .fromAutoCloseable(IO(zip.getInputStream(entry)))
          .use(is => IO(ClassReader(is)))
      }

  def transform(
      output: Path
  )(transform: ClassTransformer): Stream[IO, Unit] = {
    val acquire = IO(ZipOutputStream(Files.newOutputStream(output)))
    for
      output <- Stream.resource(Resource.fromAutoCloseable(acquire))
      _ <- classes.evalMap { clazz =>
        val writer = ClassWriter(clazz, Opcodes.V9)
        val path = s"${transform.name(clazz.getClassName)}.class"
        clazz.accept(transform.visitor(writer), ClassReader.SKIP_DEBUG)

        IO(output.putNextEntry(ZipEntry(path))).attempt.flatMap {
          case Left(ex) =>
            IO(println(s"Failed to create $path: $ex"))
          case Right(()) =>
            IO(output.write(writer.toByteArray()))
        }
      }
    yield ()
  }
}

object Jar {
  def apply(path: Path): Resource[IO, Jar] =
    Resource.fromAutoCloseable(IO(ZipFile(path.toFile))).map(new Jar(_))
}

trait ClassTransformer {
  def visitor(visitor: ClassVisitor): ClassVisitor
  def name(previous: String): String = previous
}
