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

class VOPacker (out:OutputStream) extends Packer(out) with ValuePacker
{
  import prime.vo.{ValueObjectCompanion, ValueObjectManifest, ValueObject}

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

  final def pack(ref: Ref[_ <: ValueObject]) {
    if (ref.vo_! != null) pack(ref.vo_!)
    else ref.ref match {
      case id:ObjectId => pack(id)
      case ref         => pack(ref)
    }
  }

  final def pack(refArray: RefArray[_ <: ValueObject]) {
    pack(refArray.voArray)
  }

  final def pack[V <: ValueObject](voArray:Array[V]) {
    packArray(voArray.length)
    for (item <- voArray) pack(item);
  }


  /** Packs specific valueobject fields as set in the fields bitmask.  */
  final def packValueObject(vo : ValueObject, manifest : ValueObjectManifest[_], fields : Int)
  {
    var mixin = 0;
    for (m <- manifest.mixins) if ((fields & m.fieldIndexMask) != 0) mixin += 1;
    assert(mixin <= 7, "Packing ValueObjects with more than 7 mixins is currently not supported.");

    var fieldBits = fields >>> manifest.mixinIndexBitsReserved;

    packValueObjectHeader(manifest.ID, mixin, fieldBits);

    var i = 0;
    while (fieldBits != 0 && i < manifest.lastFieldIndex)
    {
      // values are packed in groups of 8
      if (i % 8 == 0) {
        if (i > 0) fieldBits >>>= 8;
        writeByte( fieldBits );
      }
      if ((fields & (1 << i)) != 0)
        pack( manifest(i).get(vo) );

      i += 1;
    }

    if (mixin != 0) for (m <- manifest.mixins) {
      mixin = fields & m.fieldIndexMask;
      if (mixin != 0)
        packValueObject(vo, m.manifest, mixin >>> m.indexBitsReserved);
    }
  }
}
