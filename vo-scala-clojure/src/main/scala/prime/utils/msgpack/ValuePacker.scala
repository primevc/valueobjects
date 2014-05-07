package prime.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import org.bson.types.ObjectId
import clojure.lang.{IPersistentMap, IMapEntry, ISeq, Seqable}
import prime.types.{Conversion, VORef, Ref, RefArray, EmailAddr, EnumValue, RGBA, FileRef, URI, URL, DateTime, Date}
import prime.vo._
import Util._

/**
 * Prime Value msgpack writer
 *
 * When compact is true:
 * - FileRef will be packed as hash bytes only
 *
 * When compact is false:
 * - FileRef will be packed as fully prefixed hash URI-String
 */
abstract class ValuePacker(out:OutputStream, compact:Boolean = false) extends Packer(out)
{
  def pack(vo : ValueObject);
  def pack(vo : mutable.ValueObject);

  final def pack(v : EnumValue) {
    if (v.isInstanceOf[scala.Product]) super.pack(v.toString)
    else                               super.pack(v.value)
  }

  final def pack(ref: VORef[_ <: ValueObject]) {
    if (ref.isDefined) pack(ref.get.asInstanceOf[ValueObject] /* force correct overload */)
    else               pack(ref._id)
  }

  final def pack(ref: Ref[_ <: mutable.ValueObjectWithID]) {
    if (ref.vo_! != null) pack(ref.vo_!.asInstanceOf[mutable.ValueObject])
    else ref.ref match {
      case id:ObjectId => pack(id)
      case v => pack(v)
    }
  }

  final def pack(refArray: RefArray[_ <: ValueObject]) {
    pack(refArray.voArray)
  }

  final def pack(map : IPersistentMap) {
    val entries = if (map != null) map.seq else null;
    packMap(if (entries != null) entries.count else 0);
    var item = entries;
    while (item != null) {
      val entry = item.first.asInstanceOf[IMapEntry];
      pack(entry.`key`);
      pack(entry.`val`);
      item = item.next;
    }
  }

  final def pack(list : ISeq) {
    packArray(if (list != null) list.count else 0);
    var item = list;
    while (item != null) {
      pack(item.first);
      item = item.next;
    }
  }

  final def pack(list : Seqable) {
    if (list != null) pack(list.seq) else packNil();
  }

  final def writeByte(v : Int) {
    out.write(v.asInstanceOf[Byte]);
  }

  final def pack(id : ObjectId)
  {
    out.write(0xD7)
    out.write(0x1D)
    out.write(id.toByteArray);
  }

  final def pack(v : IndexedSeq[_]) {
    packArray(v.length)
    for (item <- v) pack(item);
  }

  final def pack(fileRef : FileRef) {
    if (fileRef == null) packNil();
    else if (!compact) packString(fileRef.prefixedString);
    else packString(fileRef.toString);
    /* if (fileRef.hash == null) packString(fileRef.uri);
    else {
      out.write(0xD7)
      if (fileRef.uri != null) {
        val uriBytes = fileRef.uri.getBytes("UTF-8");
        out.write(0x1F)
        out.write(uriBytes.length)
        out.write(uriBytes)
      }
      else
        out.write(0x0F)
      out.write(fileRef.hash)
    }*/
  }

  final def pack(uri     : URI)                     { super.pack(Conversion.String(uri)); }
  final def pack(url     : URL)                     { super.pack(Conversion.String(url)); }
  final def pack(email   : EmailAddr)               { super.pack(email.toString);         }
  final def pack(rgba    : RGBA)                    { super.pack(rgba.rgba);              }

  final def pack(date : org.joda.time.DateTime)     { pack( date.toInstant );       }
  final def pack(date : org.joda.time.DateMidnight) { pack( date.toInstant );       }
  final def pack(date : org.joda.time.Instant)      { super.pack( date.getMillis ); }

  final def pack(fileRefArray: Array[FileRef]) {
    packArray(fileRefArray.length)
    for (item <- fileRefArray) pack(item);
  }

  final override def pack(v : Int)     = super.pack(v);
  final override def pack(v : Double)  = super.pack(v);
  final override def pack(v : String)  = super.pack(v);
  final override def pack(v : Boolean) = super.pack(v);

  override def pack(any : Any) : this.type = { any match {
    case v : String                   => super.pack(v);
    case v : Number                   => super.pack(v);
    case v : ObjectId                 => pack(v);
    case v : DateTime                 => pack(v);
    case v : Date                     => pack(v);
    case v : URI                      => pack(v);
    case v : URL                      => pack(v);
    case v : FileRef                  => pack(v);
    case v : RGBA                     => pack(v);
    case v : EmailAddr                => pack(v);
    case v : ValueObject              => pack(v);
    case v : VORef[_]                 => pack(v.asInstanceOf[VORef[_ <: ValueObject]]);
    case v : Ref[_]                   => pack(v.asInstanceOf[Ref[_ <: mutable.ValueObjectWithID]]);
    case v : EnumValue                => pack(v);
    case v : mutable.ValueObject      => pack(v);
    case v : IndexedSeq[_]            => pack(v);
    case v : IPersistentMap           => pack(v);
    case v : Seqable                  => pack(v);
    case null                         => packNil();
    case v                            => super.pack(v);
  }; this }

  final def packValueObjectHeader(voType : Int, mixins : Int, fieldFlagBytes : Int)
  {
    val firstByte:Int = mixins << 3 | bytesUsedInInt(fieldFlagBytes)

    // pack ValueObject header
    if (voType <= 255) {
      castBytes(0) = (firstByte | 0x80).byteValue
      castBytes(1) = voType.byteValue
      out.write(castBytes, 0, 2);
      //println("packValueObjectHeader(voType: %1$3s [0x%1$h], mixins: %2$s [0x%2$h], fieldFlagBytes: 0x%3$2h) => %4$2h %5$2h".format(voType, mixins, fieldFlagBytes, castBytes(0) & 0xFF, castBytes(1)))
    } else {
      castBytes(0) = (firstByte | 0xC0).byteValue
      castBytes(1) = (voType >> 8).byteValue
      castBytes(2) = voType.byteValue
      out.write(castBytes, 0, 3);
    }
  }
}

object Util {
  final def bytesUsedInInt (n:Int) : Byte = {
         if ( n         == 0) 0
    else if ((n >>>  8) == 0) 1
    else if ((n >>> 16) == 0) 2
    else if ((n >>> 24) == 0) 3
    else                      4
  }
}
