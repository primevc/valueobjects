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

class VOPacker (out:OutputStream) extends ValuePacker(out)
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
      packValueObject(vo, vo.voManifest, vo.voIndexSet);
    }
    else packNil();
  }

  /** Packs specific valueobject fields as set in the fields bitmask.  */
  final def packValueObject(vo : ValueObject, manifest : ValueObjectManifest[_], fields : Int)
  {
    val mixinCount = manifest.mixinCount(fields);
    assert(mixinCount <= 7, "Packing ValueObjects with more than 7 mixins is currently not supported.");

    val fieldBits = fields >>> manifest.mixinIndexBitsReserved
    packValueObjectHeader(manifest.ID, mixinCount, fieldBits);
    if (fieldBits != 0) packValueObjectFields(vo, manifest, fieldBits);

    if (mixinCount > 0) for (m <- manifest.mixins)
    {
      val mixin = m.indexBitsShifted(fields);
      if (mixin != 0) {
          packValueObjectHeader(m.manifest.ID, 0, mixin);
          packValueObjectFields(vo, m.manifest, mixin);
      }
    }
  }

  final def packValueObjectFields(vo : ValueObject, manifest : ValueObjectManifest[_], fields : Int)
  {
    var i = manifest.mixinIndexBitsReserved;
    var fieldBits = fields;
    do {
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
    while (fieldBits != 0 && i < manifest.lastFieldIndex);
  }
}
