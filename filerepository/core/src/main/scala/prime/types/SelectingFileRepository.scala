package prime.types

import java.io._

class SelectingFileRepository(repositories: Map[String, FileRepository],
                              default: Option[FileRepository])
extends FileRepository
{

  private def findRepository(ref: FileRef): FileRepository = {
    repositories.get(ref.prefix).orElse(default).get
  }

  def existsImpl(ref: FileRef) = findRepository(ref).exists(ref)

  def toURI(ref: FileRef) = findRepository(ref).toURI(ref)

  def stream(ref: FileRef) = findRepository(ref).stream(ref)

  def getFile(ref: FileRef) = findRepository(ref).getFile(ref)

  def store[T](writer: OutputStream => T): FileRef = ???

  def absorb(file: File): FileRef = ???

  def absorb(is: InputStream): FileRef = ???

  private def ??? : Nothing = throw new UnsupportedOperationException("not implemented")

}
