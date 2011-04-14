package primevc.types;
 import org.apache.commons.codec.binary.Base64
 import org.bouncycastle.crypto.digests.SHA256Digest
 import org.bouncycastle.crypto.io.DigestOutputStream
 import org.bson.types.ObjectId
import java.io.{File, FileOutputStream, OutputStream}
import primevc.utils.ConvertTo

class FileRef private[types](ref:String, val hash:Array[Byte])
{
  require(ref != null || hash != null)

  override lazy val toString = if (hash == null) ref else {
    val base64 = Base64.encodeBase64URLSafeString(hash)
    if (ref == null) base64 else ref + base64
  }

  lazy val toURI = ConvertTo.uri(toString)
}

object FileRef
{
  def apply(s:String)   : FileRef = new FileRef(s, null)
  def apply(o:ObjectId) : FileRef = new FileRef("^", o.toByteArray)

  def apply(out : OutputStream) : FileRef.Builder = new FileRef.Builder(out, null)
  def apply(out : OutputStream, prefix : String) : FileRef.Builder = new FileRef.Builder(out, prefix)

  /*
  def apply() : (File, FileRef.Builder) = {
    val file = File.createTempFile("orb-", item.media.id.toString);
    (file, apply(file))
  }
  */

  def apply(file : File) : FileRef.Builder = {
    val fileOutput = new FileOutputStream(file)
    new Builder(fileOutput, null)
  }

  class Builder(wrapAround : OutputStream, prefix : String) {
    private val ref    = new FileRef(prefix, new Array[Byte](256))
    private val sha256 = new SHA256Digest
    val output = new DigestOutputStream(wrapAround, sha256)

    /**
     * Closes the output stream and returns the FileRef constructed for the data that was written.
     */
    def get : FileRef = {
      sha256.doFinal(ref.hash, 0)
      output.close()
      ref
    }
  }

  implicit def builderToOutput(b:Builder):OutputStream = b.output
}
