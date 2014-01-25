package prime.types

import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io._
import java.nio.file.{Files, StandardCopyOption}
import java.security.{DigestOutputStream, DigestInputStream, MessageDigest}


/* Helper classes. */

trait FileRefStream {
  val sha: MessageDigest
  val prefix: String
  val wrap: Closeable

  def ref(): FileRef = {
    val innerRef = new FileRef(prefix, sha.digest)
    wrap.close()
    innerRef
  }
}

class FileRefOutputStream(val wrap: OutputStream, val prefix: String,
                          val sha: MessageDigest)
extends DigestOutputStream(wrap, sha)
with FileRefStream {
  def this(wrap: OutputStream, prefix: String) =
    this(wrap, prefix, MessageDigest.getInstance("SHA-256"))
}

class FileRefInputStream(val wrap: InputStream, val prefix: String,
                         val sha: MessageDigest)
extends DigestInputStream(wrap, sha)
with FileRefStream {
  def this(wrap: InputStream, prefix: String) =
    this(wrap, prefix, MessageDigest.getInstance("SHA-256"))
}


/* Public traits. */

/** The public functions of any FileRepository. File repositories that want
  * to have garbage collection, should implement GarbageCollectableFR below.
  */
trait FileRepository {

  /** Test whether the specified FileRef exists in the repository. If the
    * repository implements GarbageCollectableFR, it uses the garbage collection
    * Storm topology. Otherwise, it calls `existsImpl` directly.
    * @param ref The FileRef to test.
    */
  final def exists(ref: FileRef): Boolean = this match {
    case gc: GarbageCollectableFR => existsImpl(ref) // TODO: invoke Storm
    case fr: FileRepository       => existsImpl(ref)
  }

  /** The implementation that actually checks whether the file as specified
    * by the supplied FileRef exists. Do not call this function directly, but
    * use `exists`!
    * @param ref The FileRef to check.
    */
  protected def existsImpl(ref: FileRef): Boolean

  /** Create an URI for the specified URI.
    * @param fileRef The FileRef to get the URI from.
    */
  def toURI(ref: FileRef): URI

  /** Absorb the specified File into the repository. The specified File is
    * deleted after it is absorbed. A FileRef to the new location of File in
    * the repository is returned.
    *
    * @param file The File to absorb.
    */
  def absorb(file: File): FileRef

  /** Absorb the content given by the specified IntputStream. The inputstream is
    * closed after it is absorbed. A FileRef to the location of the stored
    * content is returned.
    *
    * @param is The InputStream with the contents of the file.
    */
  def absorb(is: InputStream): FileRef

  /** Open an InputStream to the file as referenced by the FileRef.
    * @param ref The FileRef to stream.
    */
  def stream(ref : FileRef): InputStream

  /** Store a file using a function that receives an OutputStream to the new
    * file in the repository. The FileRef to the newly written file is returned.
    * Tests have shown that using the given OutputStream can be slow, so using
    * the absorb function is preferred.
    * @param writer The function that writes the data to the OutputStream.
    */
  def store[T](writer: OutputStream => T): FileRef

  /** Return a [[java.io.File]] for the specified [[prime.types.FileRef]] */
  def getFile(ref: FileRef): File

  /** Performs a getFile operation on this FileRepository. */
  def apply(ref: FileRef) = getFile(ref)
}

object FileRepository {

  /** Automatically convert a FileRef to an URI. */
  implicit def FileRef2URI(ref: FileRef)(implicit repository: FileRepository): URI =
    repository.toURI(ref);
}

/** This trait must be implemented by file repositories that want to support
  * garbage collection. The functions in this trait should not be called
  * directly! The garbage collector uses these functions. See the
  * wiki page on GitHub for more details.
  */
trait GarbageCollectableFR extends FileRepository {

  /** Delete the specified FileRef.
    * @param ref The FileRef to delete.
    */
  def delete(ref: FileRef): Unit
}


/* Helper classes for local file handling. */

object LocalFileRef {
  def apply(file: File, prefix : String): FileRef =
    new FileRef(prefix, DigestUtils.sha256(new FileInputStream(file)), file.getName);
}


trait LocalFileRepository extends FileRepository {

  val root: File
  val prefix: String

  // Default implementations.

  def toURI(f: FileRef) = Conversion.URI(f.toString)
  def stream(instance: FileRef) = new FileInputStream(getFile(instance))
  def existsImpl(instance: FileRef) = getFile(instance).exists

  def absorb (file : File) = {
    val ref = LocalFileRef(file, prefix)
    val newFile = getFile(ref)
    if (newFile.exists) {
      org.apache.commons.io.FileUtils.touch(newFile)
      file.delete
    }
    else file renameTo newFile
    assert(newFile.exists, "Absorbing "+ file.getAbsolutePath +" failed. Target: " + newFile.getAbsolutePath);
    ref
  }

  def absorb(is: InputStream) = {
    val tmp = File.createTempFile("local-", ".tmp", root)
    val path = tmp.toPath()
    val fris = new FileRefInputStream(is, prefix)
    Files.copy(fris, path, StandardCopyOption.REPLACE_EXISTING)
    val ref = fris.ref
    val newFile = getFile(ref)
    if (newFile.exists) {
      org.apache.commons.io.FileUtils.touch(newFile)
      tmp.delete
    }
    else tmp renameTo newFile
    assert(newFile.exists, "Absorbing "+ tmp.getAbsolutePath +" failed. Target: " + newFile.getAbsolutePath);
    ref
  }

  def store[T](writer: OutputStream => T) = {
    val tmp = File.createTempFile("local-", ".tmp", root)
    val fros = new FileRefOutputStream(new FileOutputStream(tmp), prefix)
    writer(fros)
    val ref = fros.ref
    val newFile = getFile(ref)
    if (newFile.exists) {
      org.apache.commons.io.FileUtils.touch(newFile)
      tmp.delete
    }
    else tmp renameTo newFile
    assert(newFile.exists, "Absorbing "+ tmp.getAbsolutePath +" failed. Target: " + newFile.getAbsolutePath);
    ref
  }

}


/* Basic repository implementation. */

/** This repository is used for reading and writing to an arbritary location on
  * the file system. It is not usable for production scenarios involving
  * multiple hosts, but it is very suitable for testing scenarios.
  *
  * @param root The directory where to store and read the files.
  */
class BasicLocalFileRepository(val root: File) extends LocalFileRepository {

  // Initialisation.

  root.mkdir();
  require(root.isDirectory, root + " is not a directory.")

  // Overriding definitions.

  val prefix = null;

  def getFile(f: FileRef) = new File(root.getAbsolutePath + "/" + f.toString)
}
