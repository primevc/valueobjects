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

object FileRef extends Function1[Any,FileRef]
{
  import Conversion._
  import ClojureProtocolVars._
  import org.msgpack.`object`._

  def apply(value:FileRef)      : FileRef = value;
  def apply(value:Array[Byte])  : FileRef = new FileRef(null, value);
  def apply(value:String)       : FileRef = new FileRef(String(value), null);
  def apply(value:RawType)      : FileRef = FileRef(value.asString);
  def apply(value:URI)          : FileRef = FileRef(value.toString);
  def apply(value:java.net.URI) : FileRef = FileRef(value.toString);
  def apply(value:java.net.URL) : FileRef = FileRef(value.toString);

  def apply(value:Any) : FileRef = unpack(value) match {
    case v:FileRef      => v
    case v:Array[Byte]  => FileRef(v)
    case v:String       => FileRef(v)
    case v:URI          => FileRef(v)
    case v:java.net.URI => FileRef(v)
    case v:java.net.URL => FileRef(v)
    case v:RawType      => FileRef(v)
    case None           => throw NoInputException;
    case value          => val v = file_ref.invoke(value); if (v != null) v.asInstanceOf[FileRef] else throw FailureException;
  }
}
