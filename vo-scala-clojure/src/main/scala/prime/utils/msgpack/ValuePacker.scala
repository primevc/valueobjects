package prime.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import org.bson.types.ObjectId
import prime.types.{Ref, RefArray, Enum, RGBA, FileRef, URI}
import Util._

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 04-01-11
 * Time: 15:01
 * To change this template use File | Settings | File Templates.
 */

trait ValuePacker
{
  this: Packer =>

  final def writeByte(v : Int) {
    out.write(v.asInstanceOf[Byte]);
  }

  final def pack(id : ObjectId)
  {
    out.write(0xD7)
    out.write(0x1D)
    out.write(id.toByteArray);
  }

  final def pack(fileRef  : FileRef) : Unit = pack(fileRef.toString);
  final def pack(uri      : URI)     : Unit = pack(uri.getEscapedURIReference);
  final def pack(rgba     : RGBA)    : Unit = pack(rgba.rgba);

  final def pack(date : org.joda.time.DateTime): Unit = pack( date.toInstant )
  final def pack(date : org.joda.time.DateMidnight): Unit = pack( date.toInstant )

  final def pack(date : org.joda.time.Instant) {
    pack( date.getMillis )
  }

  final def pack(fileRefArray: Array[FileRef]) {
    packArray(fileRefArray.length)
    for (item <- fileRefArray) pack(item);
  }

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
         if (n ==      0x000000 ) 0
    else if (n == (n & 0x0000FF)) 1
    else if (n == (n & 0x00FFFF)) 2
    else if (n == (n & 0xFFFFFF)) 3
    else                          4
  }
}
