package primevc.utils.msgpack

import primevc.core.traits.{VOCompanion, ValueObject}
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
class VOUnpacker(voCompanionMap : IntMap[VOCompanion[_]]) extends UnpackerImpl.VOHelper {
  def newObject = new UnpackerImpl.VOInstance {

  var data : MessagePackObject = _

  // ---
  // Value Types

  def processValueType(typeID: Int) = typeID match {
    case 0x1D => 12
  }

  def putValue(bytes: Array[Byte], startIndex: Int): Unit = data = bytes(startIndex) match {
    case 0x1D => new MessagePackObjectId( new ObjectId(bytes.slice(startIndex + 1, startIndex + 13)) )
  }

  // ---
  // Value Objects
  protected var vo           : ValueObject = _
  protected var voc          : VOCompanion[ValueObject] = _
  protected var fields       : Int = 0;
  protected var currentIndex : Int = 0;

  def prepareValueObject(typeID: Int) {
    if (vo == null) {
      voc = voCompanionMap(typeID).asInstanceOf[VOCompanion[ValueObject]]
      vo  = voc.empty
    }
    currentIndex = voc.fieldIndexOffset(typeID)
  }

  def putValue(value: AnyRef) {
    var i = 0;
    do if ((fields & (1 << i)) == 0) i += 1;
    else {
      val field = currentIndex + i
      if (field < vo.Companion.numFields)
        vo.Companion.putValue(vo, field, value)

      fields >>>= (i + 1);
      currentIndex += (if (fields != 0) i + 1 else 8 - i); // index fixup for last bit
      return;
    }
    while (i < 8)

    assert(false);
  }

  def fieldgroupRequiresMoreValues = fields != 0

  def prepareForNext8Fields(fields: Byte) {
    this.fields = fields & 0xFF;
  }

  def getData = {
    val d = if (data != null) data else new MessagePackValueObject(vo);
    data = null; vo = null; voc = null;
    d
  }
}}

class MessagePackValueObject(val vo  : ValueObject) extends MessagePackObject {
  def messagePack(packer : Packer) = packer.asInstanceOf[VOPacker].pack(vo);
}
class MessagePackObjectId   (val oid : ObjectId)    extends MessagePackObject {
  def messagePack(packer : Packer) = packer.asInstanceOf[VOPacker].pack(oid);
}
