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
  import prime.vo.{ValueObjectCompanion, ValueObject}

  /** Packs a full ValueObject: Uses the VO fields-set bits. */
  final def pack(vo : ValueObject)
  {
    if (vo == null) packNil();
    else packValueObject(vo, vo.voIndexSet);
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
  final def packValueObject[V <: ValueObject](vo : V, fields : Int)
  {
    out.write(0xD7);

    val manifest = vo.voManifest;
    var mixins = 0;
    var fieldBits = fields >>> manifest.mixinIndexMask ;
    for (m <- manifest.mixins) {
      shift += m.fieldIndexMask
      if ((set & m.fieldIndexMask) != 0) mixins += 1;
    }

    packValueObjectHeader(manifest.ID, mixins, 0);
  }
}
