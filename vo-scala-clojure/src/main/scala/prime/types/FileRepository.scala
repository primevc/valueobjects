package prime.types;
 import org.bouncycastle.crypto.digests.SHA256Digest
 import org.bouncycastle.crypto.io.DigestOutputStream
 import org.bouncycastle.crypto.Digest
 import org.apache.commons.codec.digest.DigestUtils
 import java.io._



trait FileRefOutputStream extends OutputStream {

  /** Closes the OutputStream and returns the FileRef constructed. Use this
    * function _after_ the data has been written.
    * @return A FileRef to the written file in the repository./
    */
  def ref: FileRef
}

trait FileRepository {
  def exists (ref : FileRef): Boolean
  def toURI  (ref : FileRef): URI
  def create (): FileRefOutputStream
  def absorb (file : File): FileRef
  def stream (ref : FileRef): InputStream

  def store[T](writer: FileRefOutputStream => T): FileRef = {
    val out = create
    writer(out)
    out.ref
  }
}

trait LocalFileRepository extends FileRepository {
  def getFile(instance: FileRef): File
  def apply(instance: FileRef) = getFile(instance)
  def stream(instance: FileRef) = new FileInputStream(getFile(instance))
  def exists(instance: FileRef) = getFile(instance).exists
}

class BasicLocalFileRepository(val root:File) extends LocalFileRepository {
  root.mkdir();
  require(root.isDirectory, root + " is not a directory.")

  def toURI   (f : FileRef) = Conversion.URI(f.toString)
  def getFile (f : FileRef) = new File(root.getAbsolutePath + "/" + f.toString)

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

  def create = {
    val tmpFile = File.createTempFile("basiclocal-", ".tmp", root)
    val tmpFileOS = new FileOutputStream(tmpFile)
    new DigestedFileRefOutputStream(tmpFileOS, null)
  }
}

object LocalFileRef {
  def apply(file: File): FileRef = apply(file, null);
  def apply(file: File, prefix : String): FileRef =
    new FileRef(prefix, DigestUtils.sha256(new FileInputStream(file)), file.getName);
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


  /** Create an URI for the specified URI.
    * @param fileRef The FileRef to get the URI from.
    */
  def toURI(fileRef: FileRef) = Conversion.URI(fileRef.toString)

  /** Return the [[java.io.File]] where the specified [[prime.types.FileRef]]
    * can be found on this host.
    *
    * @param f The FileRef to get a File object for. Note that this must be a
               FileRef with the `nfs: ` scheme for its URI.
    */
  def getFile (fileRef: FileRef) = {
    val fileRefURI = toURI(fileRef)
    require(fileRefURI.getScheme == "nfs", "Cannot read a non-NFS FileRef from an NFS repository.")
    val filePath = nfsMountsRoot.getAbsolutePath + "/" + fileRefURI.getHost +
                   "/" + fileRefURI.getPath
    new File(filePath)
  }

  /** Opens an [[prime.types.FileRefOutputStream]] to write a new file to, which
    * can reture the [[prime.types.FileRef]] reference to it.
    */
  def create = {
    val tmpFile = File.createTempFile("nfs-", ".tmp", myRoot)
    val tmpFileOS = new FileOutputStream(tmpFile)
    new DigestedFileRefOutputStream(tmpFileOS, myPrefix)
  }

  /** Absorb the specified File into this NFS repository. The specified File is
    * deleted after it is absorbed. A FileRef to the new location of File in
    * this NFS repository is returned.
    *
    * @param file The File to absorb.
    */
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

class DigestedFileRefOutputStream private(wrap: DigestOutputStream, prefix: String, sha256: SHA256Digest)
    extends FilterOutputStream(wrap)
    with FileRefOutputStream {

  def this(wrap: OutputStream, prefix: String, sha256: SHA256Digest) =
    this(new DigestOutputStream(wrap, sha256), prefix, sha256)

  def this(wrap: OutputStream, prefix: String) =
    this(wrap, prefix, new SHA256Digest)

  private val innerRef = new FileRef(prefix, new Array[Byte](32))

  def ref = {
    sha256.doFinal(innerRef.hash, 0)
    wrap.close()
    innerRef
  }
}
