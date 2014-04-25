package prime.types
 import org.apache.commons.codec.binary.Base64
 import prime.vo.util.ClojureFn;

case class FileRef private[prime](var _uri:String, var _hash:Array[Byte], originalName : String = null, prefix:String = "")
{
  require(_uri != null || hash != null, "either 'uri' or 'hash' should be set")
  require(prefix.length < 255, "prefix length should be < 255, but is a " + prefix.length + " character long string: " + prefix)

  private def unprefixedURI = if (prefix.length == 0) _uri else _uri.substring(prefix.length)

  def hash = if (_hash != null) _hash else {
    this._hash = Base64.decodeBase64(this.unprefixedURI);
    this._hash;
  }

  override def toString = if (_uri != null) unprefixedURI else {
    this._uri = prefix + Base64.encodeBase64URLSafeString(hash);
    unprefixedURI;
  }

  def prefixedString = if (_uri != null) _uri else { toString; this._uri; }

  override def equals(other : Any) = other match {
    case other@FileRef(u,h,_,_) => u == this._uri || h == this._hash || other.toString == this.toString
    case _ => false
  }
}

object FileRef extends Function1[Any,FileRef] with ClojureFn
{
  import Conversion._
  import ClojureProtocolVars._
  import org.msgpack.`object`._

  def apply(prefix:String, value:Array[Byte], originalName:String) : FileRef = new FileRef(null, value, originalName, if (prefix != null) prefix else "");

  def apply(value:FileRef)      : FileRef = value;
  def apply(value:Array[Byte])  : FileRef = new FileRef(null, value);
  def apply(value:RawType)      : FileRef = FileRef(value.asString);
  def apply(value:URI)          : FileRef = FileRef(value.toString);
  def apply(value:URL)          : FileRef = FileRef(value.toString);
  def apply(value:String)       : FileRef = value.lastIndexOf('/') match {
    case -1 =>
      new FileRef(value, null);
    case n =>
      new FileRef(value, null, null, value.substring(0, n + 1 /* '/' length */));
  }

  def apply(value:Any) : FileRef = unpack(value) match {
    case v:FileRef      => v
    case v:Array[Byte]  => FileRef(v)
    case v:String       => FileRef(v)
    case v:URI          => FileRef(v)
    case v:URL          => FileRef(v)
    case v:RawType      => FileRef(v)
    case None           => throw NoInputException;
    case value          => val v = file_ref.invoke(value); if (v != null) v.asInstanceOf[FileRef] else throw FailureException;
  }


  //
  // Clojure IFn
  //
  override def invoke(any : AnyRef) : AnyRef = apply(any)

  final def applyTo (arglist: clojure.lang.ISeq) = clojure.lang.RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case n => throwArity(n);
  }
}
