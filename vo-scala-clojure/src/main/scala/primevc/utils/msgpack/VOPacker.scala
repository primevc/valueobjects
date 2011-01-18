package primevc.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import primevc.core.traits.{VOMessagePacker, VOCompanion, ValueObject}
import org.bson.types.ObjectId
import java.net.URI
import primevc.types.{Enum, RGBA, FileRef}

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
    val time    = id._time
    val machine = id._machine
    val inc     = id._inc

    val bytes = Array(0xD7 toByte, 0x1D toByte,
      time    & 0xFF toByte, (time    >>> 8) & 0xFF toByte, (time    >>> 16) & 0xFF toByte, (time    >>> 24) & 0xFF toByte,
      machine & 0xFF toByte, (machine >>> 8) & 0xFF toByte, (machine >>> 16) & 0xFF toByte, (machine >>> 24) & 0xFF toByte,
      inc     & 0xFF toByte, (inc     >>> 8) & 0xFF toByte, (inc     >>> 16) & 0xFF toByte, (inc     >>> 24) & 0xFF toByte
    );
    out.write(bytes);
  }

  def pack(fileRef  : FileRef) : Unit = pack(fileRef.toString);
  def pack(uri      : URI)     : Unit = pack(uri.toString)
  def pack(rgba     : RGBA)    : Unit = pack(rgba.argb)

  /** Packs a full ValueObject: Updates the VO fields-set bits, and uses those. */
  def pack[V <: ValueObject](vo : V)
  {
    vo.updateFieldsSet_!;
    packValueObject(vo, vo.$fieldsSet)
  }

  /** Packs specific valueobject fields as set in the fields bitmask.  */
  def packValueObject[V <: ValueObject](vo : V, fields : Int)
  {
    out.write(0xD7);
    vo.Companion.asInstanceOf[VOMessagePacker[V]].msgpack_packVO(this, vo, fields);
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
