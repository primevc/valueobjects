package prime.types;
 import org.apache.commons.codec.binary.Base64
 import org.bouncycastle.crypto.digests.SHA256Digest
 import org.bouncycastle.crypto.io.DigestOutputStream
 import org.bouncycastle.crypto.Digest
 import org.apache.commons.codec.digest.DigestUtils
 import java.io._


case class FileRef private[prime]( val _ref:String, val _hash:Array[Byte], val originalName : String = null)
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
  def apply(b:Array[Byte]) : FileRef = new FileRef(null, b)
  def apply(s:String)      : FileRef = new FileRef(s, null)
  def apply(uri:URI)       : FileRef = new FileRef(uri.toString, null)
  def apply(o:ObjectId)    : FileRef = new FileRef("^", o.toByteArray)

  def apply(file : File)   : FileRef = apply(file, null)
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
