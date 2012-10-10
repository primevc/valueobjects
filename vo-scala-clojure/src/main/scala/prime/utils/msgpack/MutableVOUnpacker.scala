package prime.utils.msgpack

import prime.vo.mutable.{VOCompanion, ValueObject}
import collection.immutable.IntMap
import org.msgpack.{Packer, MessagePackObject, UnpackerImpl}
import org.bson.types.ObjectId

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 06-01-11
 * Time: 11:21
 * To change this template use File | Settings | File Templates.
 */
class MutableVOUnpacker(voCompanionMap : IntMap[VOCompanion[_]]) extends UnpackerImpl.VOHelper {
  def newObject = new VOInstanceUnpacker(voCompanionMap)
}
object MutableVOUnpacker {
  var defaultVOCompanionMap : IntMap[VOCompanion[_]] = _
  val zerobitsLookupTable = Array(0,1,28,2,29,14,24,3,30,22,20,15,25,17,4,8,31,27,13,23,21,19,16,7,26,12,18,6,11,5,10,9)
}

class VOInstanceUnpacker(voCompanionMap : IntMap[VOCompanion[_]]) extends UnpackerImpl.VOInstance
{
  var data : MessagePackObject = _

  // ---
  // Value Types

  final def processValueType(typeID: Int) = typeID match {
    case 0x1D => 12
  }

  final def putValue(bytes: Array[Byte], startIndex: Int): Unit = data = bytes(startIndex) match {
    case 0x1D => new MessagePackObjectId( new ObjectId(bytes.slice(startIndex + 1, startIndex + 13)) )
  }

  // ---
  // Value Objects
  protected var vo          : ValueObject = _
  protected var voc         : VOCompanion[ValueObject] = _
  protected var fields      : Int = 0;
  protected var fieldOffset : Int = 0;

  final def prepareValueObject(typeID: Int) {
    if (vo == null) {
      require(voCompanionMap.contains(typeID), "Type with id: "+typeID+", not found in voCompanionMap")
      voc = voCompanionMap(typeID).asInstanceOf[VOCompanion[ValueObject]]
      vo  = voc.empty
    }
    //println("prepareValueObject: "+ vo +" @"+ typeID)
    fieldOffset = voc.fieldIndexOffset(typeID)
  }

  final def putValue(value: AnyRef) {
    // Thanks to http://graphics.stanford.edu/~seander/bithacks.html  and  https://gist.github.com/1319739
    var i = MutableVOUnpacker.zerobitsLookupTable( (fields & -fields) * 0x077CB531 >>> 27 );
    
    val field = fieldOffset + i
    //assert(vo != null, "Cannot write field:" + field + " = '" + value + "'' to null VO type:" + currentType)
    //println("putValue: %50s @ %2s in %s; i = %s, fieldOffset = %2s, fields = %6$s ".format(value,field,vo,i,fieldOffset,fields))
    vo.voCompanion.putValue(vo, field, value)

    fields ^= (1 << i);
    if (fields == 0) fieldOffset += 8 - i; // index fixup for last bit
  }

  final def fieldgroupRequiresMoreValues = fields != 0

  final def prepareForNext8Fields(fields: Byte) {
    this.fields = fields & 0xFF;
  }

  final def getData = {
    val d = if (data != null) data else new MessagePackValueObject(vo);
    data = null; vo = null; voc = null;
    d
  }
}

class MessagePackValueObject(val vo  : ValueObject) extends MessagePackObject {
  def messagePack(packer : Packer) = packer.asInstanceOf[MutableVOPacker].pack(vo);
}
class MessagePackObjectId   (val oid : ObjectId)    extends MessagePackObject {
  def messagePack(packer : Packer) = packer.asInstanceOf[MutableVOPacker].pack(oid);
}
