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
    var mixin = 0;
    for (m <- manifest.mixins) if ((fields & m.fieldIndexMask) != 0) mixin += 1;
    assert(mixin <= 7, "Packing ValueObjects with more than 7 mixins is currently not supported.");

    println("manifest = %s, vo = %s, fields = %x, fieldBits = %x, mixin = %d, mixinIndexBitsReserved = %d".format(manifest,  vo,fields,fields >>> manifest.mixinIndexBitsReserved,mixin,manifest.mixinIndexBitsReserved));

    val fieldBits = fields >>> manifest.mixinIndexBitsReserved
    packValueObjectHeader(manifest.ID, mixin, fieldBits);
    if (fieldBits != 0) packValueObjectFields(vo, manifest, fieldBits);

    if (mixin > 0) for (m <- manifest.mixins)
    {
      mixin = (fields & m.fieldIndexMask) >>> m.indexBitsShifted;
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

      if ((fieldBits & 0x01) != 0){ val v = manifest(i    ).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 0, fieldBits & 0x01, v)); pack(v); };
      if ((fieldBits & 0x02) != 0){ val v = manifest(i + 1).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 1, fieldBits & 0x02, v)); pack(v); };
      if ((fieldBits & 0x04) != 0){ val v = manifest(i + 2).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 2, fieldBits & 0x04, v)); pack(v); };
      if ((fieldBits & 0x08) != 0){ val v = manifest(i + 3).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 3, fieldBits & 0x08, v)); pack(v); };
      if ((fieldBits & 0x10) != 0){ val v = manifest(i + 4).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 4, fieldBits & 0x10, v)); pack(v); };
      if ((fieldBits & 0x20) != 0){ val v = manifest(i + 5).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 5, fieldBits & 0x20, v)); pack(v); };
      if ((fieldBits & 0x40) != 0){ val v = manifest(i + 6).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 6, fieldBits & 0x40, v)); pack(v); };
      if ((fieldBits & 0x80) != 0){ val v = manifest(i + 7).get(vo); println("%s[%s] bit %x = %s".format(manifest.getClass, i + 7, fieldBits & 0x80, v)); pack(v); };

      fieldBits >>>= 8;
      i += 8;
    }
    while (fieldBits != 0 && i < manifest.lastFieldIndex);
  }
}
