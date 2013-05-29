package prime.types

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.io.DigestOutputStream
import org.bouncycastle.crypto.Digest
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.{FileUtils, IOUtils}
import java.io._


/* Public traits. */

trait FileRefOutputStream extends OutputStream {

  /** Closes the OutputStream and returns the FileRef constructed. Use this
    * function _after_ the data has been written.
    * @return A FileRef to the written file in the repository.
    */
  def ref: FileRef
}

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
    case gc: GarbageCollectableFR => existsImpl(rnf) // TODO: invoke Storm
    case fr: FileRepository       => existsImpl(ref)
  }

  /** The implementation that actually checks whether the file as specified
    * by the supplied FileRef exists. Do not call this function directly, but
    * use `exists`!
    * @param ref The FileRef to check.
    */
  def existsImpl(ref: FileRef): Boolean

  /** Create an URI for the specified URI.
    * @param fileRef The FileRef to get the URI from.
    */
  def toURI(ref: FileRef): URI

  /** Opens an [[prime.types.FileRefOutputStream]] to write a new file to, which
    * can return the [[prime.types.FileRef]] reference to it.
    */
  def create(): FileRefOutputStream

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
    * @param writer The function that writes the data to the OutputStream.
    */
  def store[T](writer: FileRefOutputStream => T): FileRef = {
    val out = create
    writer(out)
    out.ref
  }
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


/* Helper classes. */

class DigestFileRefOutputStream private(wrap: DigestOutputStream, prefix: String, sha256: SHA256Digest)
    extends FilterOutputStream(wrap)
    with FileRefOutputStream {

  def this(wrap: OutputStream, prefix: String, sha256: SHA256Digest) =
    this(new DigestOutputStream(wrap, sha256), prefix, sha256)

  def this(wrap: OutputStream, prefix: String) =
    this(wrap, prefix, new SHA256Digest)

  def ref = {
    val innerRef = new FileRef(prefix, new Array[Byte](32))
    sha256.doFinal(innerRef.hash, 0)
    wrap.close()
    innerRef
  }
}


/* Helper classes for local file handling. */

trait LocalFileRepository extends FileRepository {

  /** Return the [[java.io.File]] where the specified [[prime.types.FileRef]]
    * can be found on this host.
    *
    * @param f The FileRef to get a File object for. Note that this must be a
    *          FileRef with the `nfs: ` scheme for its URI.
    */
  def getFile(instance: FileRef): File
  def apply(instance: FileRef) = getFile(instance)

  def stream(instance: FileRef) = new FileInputStream(getFile(instance))
  def existsImpl(instance: FileRef) = getFile(instance).exists

  def absorb(is: InputStream) = {
    val fileRefOS = this.create
    IOUtils.copy(is, fileRefOS)
    is.close
    fileRefOS.ref
  }
}

object LocalFileRef {
  def apply(file: File): FileRef = apply(file, null);
  def apply(file: File, prefix : String): FileRef =
    new FileRef(prefix, DigestUtils.sha256(new FileInputStream(file)), file.getName);
}

class LocalDigestFileRefOutputStream private(repo: LocalFileRepository, prefix: String,
                                             tmpFile: File, wrap: OutputStream)
    extends DigestFileRefOutputStream(wrap, prefix) {

  def this(repo: LocalFileRepository, prefix: String, tmpFile: File) =
    this(repo, prefix, tmpFile, new FileOutputStream(tmpFile))

  override def ref = {
    val innerRef = super.ref
    if (! repo.exists(innerRef))
      FileUtils.moveFile(tmpFile, repo.getFile(innerRef))
    innerRef
  }
}


/* Actual repository implementations. */

/** This repository is used for reading and writing to an arbritary location on
  * the file system. It is not usable for production scenarios involving
  * multiple hosts, but it is very suitable for testing scenarios.
  *
  * @param root The directory where to store and read the files.
  */
class BasicLocalFileRepository(val root:File) extends LocalFileRepository {
  root.mkdir();
  require(root.isDirectory, root + " is not a directory.")

  def toURI(f: FileRef) = Conversion.URI(f.toString)
  def getFile(f: FileRef) = new File(root.getAbsolutePath + "/" + f.toString)
  def create = new LocalDigestFileRefOutputStream(this, null,
    File.createTempFile("basiclocal-", ".tmp", root))

  def absorb (file : File) = {
    val ref = LocalFileRef(file)
    val newFile = getFile(ref)
    if (newFile.exists) {
      org.apache.commons.io.FileUtils.touch(newFile)
      file.delete
    }
    else file renameTo newFile
    assert(newFile.exists, "Absorbing "+ file.getAbsolutePath +" failed. Target: " + newFile.getAbsolutePath);
    ref
  }
}


/** This repository is used for reading and writing to NFS file repositories.
  *
  * It recognises and creates `nfs:` [[prime.types.FileRef]] objects. The idea
  * is that files are not distributed among the servers/hosts, but are loaded
  * from the host the file resides on.
  *
  * The [[prime.types.FileRef]] prefix for this repository follows the following
  * convention: `nfs:internal-hostname//repository-name/`. A file can be read
  * and written to the path `/nfsMountRoot/internal-hostname/repository-name/HASH`.
  *
  * @param nfsMountsRoot A file pointing to the root directory of all mount
  *                      points accessible by this host.
  * @param repositoryName The name of this repository.
  */
class NFSRepository(nfsMountsRoot: File, repositoryName: String)
    extends LocalFileRepository {

  import java.net.InetAddress

  require(nfsMountsRoot.exists && nfsMountsRoot.isDirectory,
    "The root directory of all NFS mounts must exist and point to a directory.")

  val myLocalHostname = InetAddress.getLocalHost().getHostName();
  val myNFSMount = new File(nfsMountsRoot, myLocalHostname)

  require(myNFSMount.exists && myNFSMount.isDirectory,
    "The NFS \"mount\" for this host, which should be located at '" +
    myNFSMount.getAbsolutePath + "', does not exist or is not a directory.")

  val myRoot = new File(myNFSMount, repositoryName)
  myRoot.mkdir

  require(myRoot.exists && myRoot.isDirectory,
    "The local repository directory, which should be located at '" +
    myRoot.getAbsolutePath + "', could not be created.")

  val myPrefix = "nfs://" + myLocalHostname + "/" + repositoryName + "/"


  def toURI(fileRef: FileRef) = Conversion.URI(fileRef.toString)
  def create = new LocalDigestFileRefOutputStream(this, myPrefix,
    File.createTempFile("nfs-", ".tmp", myRoot))

  def getFile (fileRef: FileRef) = {
    val fileRefURI = toURI(fileRef)
    require(fileRefURI.getScheme == "nfs", "Cannot read a non-NFS FileRef from an NFS repository.")
    val filePath = nfsMountsRoot.getAbsolutePath + "/" + fileRefURI.getHost +
                   "/" + fileRefURI.getPath
    new File(filePath)
  }

  def absorb (file : File) = {
    val ref = LocalFileRef(file, myPrefix)
    val refFile = getFile(ref)
    if (refFile.exists) {
      org.apache.commons.io.FileUtils.touch(refFile) // update the access time.
      file.delete
    } else {
      file.renameTo(refFile)
    }
    require(refFile.exists, "Absorbing "+ file.getAbsolutePath +" failed. Target: " +
                            refFile.getAbsolutePath);
    ref
  }
}
