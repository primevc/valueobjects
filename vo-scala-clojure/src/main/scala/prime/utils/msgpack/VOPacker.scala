package prime.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import org.bson.types.ObjectId
import prime.types.{Ref, RefArray, Enum, RGBA, FileRef, URI}
import Util._

/**
 * ValueObject msgpack writer
 *
 * See ValuePacker for compact option documentation.
 */
class VOPacker (out:OutputStream, compact:Boolean = false) extends ValuePacker(out, compact)
{
  import prime.vo.{ValueObjectCompanion, ValueObjectManifest, ValueObject}

  final def pack(vo: prime.vo.mutable.ValueObject) {
    assert(false);
  }

  /** Packs a full ValueObject: Uses the VO fields-set bits. */
  final def pack(vo : ValueObject)
  {
    if (vo != null)
    {
      out.write(0xD7); // VO msgpack type header
      packValueObject(vo, vo.voIndexSet);
    }
    else packNil();
  }

  /** Packs specific valueobject fields as set in the fields bitmask.  */
  final def packValueObject(vo : ValueObject, fields : Int)
  {
    val manifest = vo.voManifest;
    val mixinCount = manifest.mixinCount(fields);
    assert(mixinCount <= 7, "Packing ValueObjects with more than 7 mixins is currently not supported.");

    val fieldBits = fields >>> manifest.mixinIndexBitsReserved
    packValueObjectHeader(manifest.ID, mixinCount, fieldBits);
    if (fieldBits != 0) packValueObjectFields(vo, fieldBits, manifest.mixinIndexBitsReserved);

    if (mixinCount > 0) for (m <- manifest.mixins)
    {
      val mixin = m.indexBitsShifted(fields);
      if (mixin != 0) try {
          packValueObjectHeader(m.manifest.ID, 0, mixin);
          packValueObjectFields(vo, mixin, m.numberOfIndexBitsShifted);
      } catch {
        case e : Throwable =>
          throw new Exception("Error while packing "+ vo +" mixin: " + m + "; mixin: 0x" + Integer.toHexString(mixin) + " fields: 0x" + Integer.toHexString(fields), e)
      }
    }
  }

  final def packValueObjectFields(vo : ValueObject, fields : Int, startIndex : Int)
  {
    val manifest = vo.voManifest;
    var i = startIndex;
    var fieldBits = fields;
    while (fieldBits != 0) {
      writeByte( fieldBits );

      if ((fieldBits & 0x01) != 0) pack( manifest(i    ).get(vo) );
      if ((fieldBits & 0x02) != 0) pack( manifest(i + 1).get(vo) );
      if ((fieldBits & 0x04) != 0) pack( manifest(i + 2).get(vo) );
      if ((fieldBits & 0x08) != 0) pack( manifest(i + 3).get(vo) );
      if ((fieldBits & 0x10) != 0) pack( manifest(i + 4).get(vo) );
      if ((fieldBits & 0x20) != 0) pack( manifest(i + 5).get(vo) );
      if ((fieldBits & 0x40) != 0) pack( manifest(i + 6).get(vo) );
      if ((fieldBits & 0x80) != 0) pack( manifest(i + 7).get(vo) );

      fieldBits >>>= 8;
      i += 8;
    }
  }
}
