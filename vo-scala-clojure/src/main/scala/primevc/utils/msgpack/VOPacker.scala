package primevc.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import primevc.core.traits.{VOMessagePacker, VOCompanion, ValueObject}
import org.bson.types.ObjectId
import primevc.types.{Ref, RefArray, Enum, RGBA, FileRef, URI}

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 04-01-11
 * Time: 15:01
 * To change this template use File | Settings | File Templates.
 */

class VOPacker (out:OutputStream) extends Packer(out)
{
  def pack(id : ObjectId)
  {
    out.write(0xD7)
    out.write(0x1D)
    out.write(id.toByteArray);
  }

  def pack(fileRef  : FileRef) : Unit = pack(fileRef.toString);
  def pack(uri      : URI)     : Unit = pack(uri.getEscapedURIReference);
  def pack(rgba     : RGBA)    : Unit = pack(rgba.rgba);

  /** Packs a full ValueObject: Updates the VO fields-set bits, and uses those. */
  def pack(vo : ValueObject)
  {
    if (vo == null) packNil()
    else {
      vo.updateFieldsSet_!;
      packValueObject(vo, vo.$fieldsSet)
    }
  }

  def pack(date : org.joda.time.DateTime): Unit = pack( date.toInstant )
  def pack(date : org.joda.time.DateMidnight): Unit = pack( date.toInstant )

  def pack(date : org.joda.time.Instant) {
    pack( date.getMillis / 1000 )
  }

  /** Packs specific valueobject fields as set in the fields bitmask.  */
  def packValueObject[V <: ValueObject](vo : V, fields : Int)
  {
    out.write(0xD7);
    vo.Companion.asInstanceOf[VOMessagePacker[V]].msgpack_packVO(this, vo, fields);
  }

  def pack(ref: Ref[_ <: ValueObject]) {
    if (ref.ref != null) require(ref.vo_! != null)
    pack(ref.vo_!)
  }

  def pack(refArray: RefArray[_ <: ValueObject]) {
    pack(refArray.voArray)
  }

  def pack[V <: ValueObject](voArray:Array[V]) {
    packArray(voArray.length)
    for (item <- voArray) pack(item);
  }

  def packValueObjectHeader(voType : Int, mixins : Int, fieldFlagBytes : Int)
  {
    val firstByte:Int = mixins << 3 | bytesUsedInInt(fieldFlagBytes)

    // pack ValueObject header
    if (voType <= 255) {
      castBytes(0) = (firstByte | 0x80).byteValue
      castBytes(1) = voType.byteValue
      out.write(castBytes, 0, 2);
    } else {
      castBytes(0) = (firstByte | 0xC0).byteValue
      castBytes(1) = (voType >> 8).byteValue
      castBytes(2) = voType.byteValue
      out.write(castBytes, 0, 3);
    }
  }

  def writeByte(v : Int) {
    out.write(v.asInstanceOf[Byte]);
  }

  private def bytesUsedInInt (n:Int) : Byte = {
         if (n ==      0x000000 ) 0
    else if (n == (n & 0x0000FF))	1
		else if (n == (n & 0x00FFFF))	2
		else if (n == (n & 0xFFFFFF)) 3
		else                          4
  }
}
