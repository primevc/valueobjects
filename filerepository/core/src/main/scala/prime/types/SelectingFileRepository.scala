package prime.types

import java.io._
import java.util.regex.Pattern

/** A forwarding file repository, based on the prefix of the ref passed in. The prefix
  * is matched agains the regex patterns of the passed in repositories. The first
  * match is used, or an error is thrown.
  *
  * Of course, only those operations that receive a FileRef are implemented.
  */
class SelectingFileRepository(repositories: List[Tuple2[Pattern, FileRepository]])
extends FileRepository
{

  protected[types] def findRepository(ref: FileRef): FileRepository = {
    repositories.find( {case (p, fr) => p.matcher(ref.prefix).matches} ).get._2
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
