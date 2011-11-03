package primevc.types;
 import org.apache.commons.codec.binary.Base64
 import org.bouncycastle.crypto.digests.SHA256Digest
 import org.bouncycastle.crypto.io.DigestOutputStream
import primevc.utils.ConvertTo
import org.bouncycastle.crypto.Digest
import org.apache.commons.codec.digest.DigestUtils
import java.io._

trait FileRefOutputStream extends OutputStream {
  /** Closes the OutputStream and returns the FileRef constructed */
  def ref: FileRef
}

trait FileRepository {
  def exists (ref : FileRef): Boolean
  def toURI(instance : FileRef): URI
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
  require(root.isDirectory)

  def toURI   (f : FileRef) = ConvertTo.uri(f.toString)
  def getFile (f : FileRef) = new File(root.getAbsolutePath + "/" + f.toString)
  def exists  (f : FileRef) = getFile(f).exists

//  def save (stream : OutputStream) = FileRef(new StreamWrapper(stream)).get

  def absorb (file : File) = {
    val ref = FileRef(file)
    val newFile = getFile(ref)
    if (newFile.exists) {
      org.apache.commons.io.FileUtils.touch(newFile)
      file.delete
    }
    else file renameTo newFile
    
    ref
  }

  def create = new TmpFileStream()

  class TmpFileStream private[primevc](tmpFile : File, out : FileOutputStream) extends FilterOutputStream(out) with FileRefOutputStream
  {
    def this(tmpFile : File) = this(tmpFile, new FileOutputStream(tmpFile))
    def this() = this(File.createTempFile("blfr-", ".tmp", root))
    
    protected val builder = FileRef(this)

    def ref = {
      val ref = builder.get
      tmpFile renameTo getFile(ref)
      ref
    }
  }
}

class FileRef private[primevc]( val _ref:String, val _hash:Array[Byte], val originalName : String = null)
{
  require(_ref != null || _hash != null, "either ref or hash should be set")

  override lazy val toString = if (_hash == null) _ref else {
    val base64 = Base64.encodeBase64URLSafeString(_hash)
    if (_ref == null) base64 else _ref + base64
  }

  def toURI(implicit repository:FileRepository) = repository.toURI(this)
}

object FileRef
{
  def apply(s:String)   : FileRef = new FileRef(s, null)
  def apply(o:ObjectId) : FileRef = new FileRef("^", o.toByteArray)

  def apply(file : File) : FileRef = apply(file, null)
  def apply(file : File, prefix : String) : FileRef = new FileRef(prefix, DigestUtils.sha256(new FileInputStream(file)), file.getName)


  def apply(out : OutputStream) : FileRef.Builder = new FileRef.Builder(out, null)
  def apply(out : OutputStream, prefix : String) : FileRef.Builder = new FileRef.Builder(out, prefix)

  /*
  def apply() : (File, FileRef.Builder) = {
    val file = File.createTempFile("orb-", item.media.id.toString);
    (file, apply(file))
  }
  */

  class Builder(wrapAround : OutputStream, prefix : String) {
    private val ref    = new FileRef(prefix, new Array[Byte](32))
    private val sha256 = new SHA256Digest
    val output = new DigestOutputStream(wrapAround, sha256)

    /**
     * Closes the output stream, tells the FileRepository to store() and returns the FileRef constructed for the data that was written.
     */
    lazy val get : FileRef = {
      sha256.doFinal(ref._hash, 0)
      output.close()
      ref
    }
  }

  implicit def builderToOutput(b:Builder):OutputStream = b.output
}
