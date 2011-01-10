package primevc.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import primevc.core.traits.{VOMessagePacker, VOCompanion, ValueObject}

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 04-01-11
 * Time: 15:01
 * To change this template use File | Settings | File Templates.
 */

class VOPacker (out:OutputStream) extends Packer(out)
{
  /** Packs a full ValueObject: Updates the VO fields-set bits, and uses those. */
  def pack[V <: ValueObject](vo : V)
  {
    vo.updateFieldsSet();
    packValueObject(vo, vo.$fieldsSet)
  }

  /** Packs specific valueobject fields as set in the fields bitmask.  */
  def packValueObject[V <: ValueObject](vo : V, fields : Int)
  {
    out.write(0xD7);
    vo.Companion.asInstanceOf[VOMessagePacker[V]].msgpack_packVO(this, vo, fields);
  }

  def packValueObjectHeader(voType : Int, superTypes : Int, fieldFlagBytes : Int)
  {
    val firstByte:Int = superTypes << 3 | bytesUsedInInt(fieldFlagBytes)

    // pack ValueObject header
    if (voType <= 255) {
      castBytes(0) = (firstByte | 0x80).byteValue
      castBytes(1) = voType.byteValue
      out.write(castBytes, 0, 3);
    } else {
      castBytes(0) = (firstByte | 0xC0).byteValue
      castBytes(1) = (voType >> 8).byteValue
      castBytes(2) = voType.byteValue
      out.write(castBytes, 0, 4);
    }
  }

  def writeByte(v:Int) {
    out.write(v);
  }

  private def bytesUsedInInt (n:Int) : Byte = {
         if (n <= 0x0000FF)	1
		else if (n <= 0x00FFFF)	2
		else if (n <= 0xFFFFFF) 3
		else                    4
  }
}
