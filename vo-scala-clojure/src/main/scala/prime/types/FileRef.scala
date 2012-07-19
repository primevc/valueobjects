package prime.types
 import org.apache.commons.codec.binary.Base64

class FileRef private[prime](val uri:String, val hash:Array[Byte], val originalName : String = null)
{
  require(uri != null || hash != null, "either 'uri' or 'hash' should be set")

  override lazy val toString = if (hash == null) uri else {
    val base64 = Base64.encodeBase64URLSafeString(hash)
    if (uri == null) base64 else uri + base64
  }
}
