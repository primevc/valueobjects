package prime.utils.msgpack

import org.msgpack.Packer
import java.lang.Math
import java.io.OutputStream
import org.bson.types.ObjectId
import prime.types.{Conversion, Ref, RefArray, Enum, RGBA, FileRef, URI}

class MutableVOPacker (out:OutputStream, compact:Boolean = false) extends ValuePacker(out, compact)
{
  import prime.vo.mutable.{VOMessagePacker, VOCompanion, ValueObject}

  final def pack(vo: prime.vo.ValueObject) {
    assert(false);
  }

  /** Packs a full ValueObject: Updates the VO fields-set bits, and uses those. */
  final def pack(vo : ValueObject)
  {
    if (vo == null) packNil()
    else {
      vo.updateFieldsSet_!;
      packValueObject(vo, vo.$fieldsSet)
    }
  }

  /** Packs specific valueobject fields as set in the fields bitmask.  */
  final def packValueObject[V <: ValueObject](vo : V, fields : Int)
  {
    out.write(0xD7);
    //println(fields.formatted("packValueObject(" + vo + ", fields: %h)"))
    vo.voCompanion.asInstanceOf[VOMessagePacker[V]].msgpack_packVO(this, vo, fields);
  }

  final def pack[V <: ValueObject](voArray:Array[V]) {
    packArray(voArray.length)
    for (item <- voArray) pack(item.asInstanceOf[ValueObject] /* force correct overload */);
  }
}
