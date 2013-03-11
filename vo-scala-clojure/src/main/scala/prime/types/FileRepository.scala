package prime.types;
 import org.apache.commons.codec.binary.Base64
 import org.bouncycastle.crypto.digests.SHA256Digest
 import org.bouncycastle.crypto.io.DigestOutputStream
 import org.bouncycastle.crypto.Digest
 import org.apache.commons.codec.digest.DigestUtils
 import java.io._



trait FileRefOutputStream extends OutputStream {
  /** Closes the OutputStream and returns the FileRef constructed */
  def ref: FileRef
}

trait FileRepository {
  def exists (ref : FileRef): Boolean
  def toURI  (ref : FileRef): URI
  def create (): FileRefOutputStream
  def absorb (file : File): FileRef
  def stream (ref : FileRef): InputStream

  def store[T](writer: FileRefOutputStream => T): FileRef = {
    val bldr = create
    writer(bldr)
    bldr.ref
  }
}

trait LocalFileRepository extends FileRepository {
  def getFile(instance : FileRef): File
  def apply  (instance : FileRef): File = this getFile instance
  def stream (instance : FileRef): InputStream = new FileInputStream(this(instance))
}

class BasicLocalFileRepository(val root:File) extends LocalFileRepository {
  root.mkdir();
  require(root.isDirectory, root + " is not a directory.")

  def toURI   (f : FileRef) = Conversion.URI(f.toString)
  def getFile (f : FileRef) = new File(root.getAbsolutePath + "/" + f.toString)
  def exists  (f : FileRef) = getFile(f).exists

//  def save (stream : OutputStream) = FileRef(new StreamWrapper(stream)).get

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

  def create = new TmpFileStream()

  class TmpFileStream private[prime](tmpFile : File, out : FileOutputStream) extends FilterOutputStream(out) with FileRefOutputStream
  {
    def this(tmpFile : File) = this(tmpFile, new FileOutputStream(tmpFile))
    def this() = this(File.createTempFile("blfr-", ".tmp", root))
    
    protected val builder = LocalFileRef(this)

    def ref = {
      val ref = builder.get
      tmpFile renameTo getFile(ref)
      ref
    }
  }
}

object LocalFileRef
{
  def apply(file : File)   : FileRef = apply(file, null);
  def apply(file : File, prefix : String) : FileRef = new FileRef(prefix, DigestUtils.sha256(new FileInputStream(file)), file.getName);

  def apply(out : OutputStream)                  : Builder = new Builder(out, null);
  def apply(out : OutputStream, prefix : String) : Builder = new Builder(out, prefix);

  class Builder(wrapAround : OutputStream, prefix : String)
  {
    private val ref    = new FileRef(prefix, new Array[Byte](32))
    private val sha256 = new SHA256Digest
    val output = new DigestOutputStream(wrapAround, sha256)

    /**
     * Closes the output stream, tells the FileRepository to store() and returns the FileRef constructed for the data that was written.
     */
    lazy val get : FileRef = {
      sha256.doFinal(ref.hash, 0)
      output.close()
      ref
    }
  }

  implicit def builderToOutput(b:Builder):OutputStream = b.output
}
