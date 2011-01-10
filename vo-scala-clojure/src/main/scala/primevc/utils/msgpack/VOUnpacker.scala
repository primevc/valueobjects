package primevc.utils.msgpack

import primevc.core.traits.{VOCompanion, ValueObject}
import collection.immutable.IntMap
import org.msgpack.{Packer, MessagePackObject, UnpackerImpl}

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 06-01-11
 * Time: 11:21
 * To change this template use File | Settings | File Templates.
 */
class VOUnpacker(voCompanionMap : IntMap[VOCompanion[_]], voValueMap : IntMap[SpecialValueType]) extends UnpackerImpl.VOHelper {
  def newObject = new UnpackerImpl.VOInstance {

  var data : MessagePackObject = _

  // ---
  // Value Types
  var special : SpecialValueType = _

  def processValueType(typeID: Int) = {
    val s = voValueMap(typeID);
    this.special = s;
    s.bytes
  }

  def putValue(bytes: Array[Byte], startIndex: Int) {
    data = special.factory(bytes, startIndex)
  }

  // ---
  // Value Objects
  protected var vo           : ValueObject = _
  protected var voc          : VOCompanion[ValueObject] = _
  protected var fields       : Byte = 0;
  protected var currentBit   : Int = 0;
  protected var currentIndex : Int = 0

  def prepareValueObject(typeID: Int) {
    if (vo == null) {
      voc = voCompanionMap(typeID).asInstanceOf[VOCompanion[ValueObject]]
      vo  = voc.empty
    }
    currentIndex = voc.fieldIndexOffset(vo, typeID)
  }

  def putValue(value: AnyRef) {
    var i = currentBit;
    do if ((fields & (1 << i)) != 0) {
      vo.Companion.putValue(vo, currentIndex + i, value)
      return;
    }
    else  i += 1;
    while (i < 7)

    assert(false);
  }

  def fieldgroupRequiresMoreValues = currentBit != 7

  def prepareForNext8Fields(fields: Byte) {
    this.fields = fields;
    this.currentBit = if (fields != 0) 0 else 7; // 7 == done
  }

  def getData = {
    val d = data;
    data = null; vo = null; voc = null;
    d
  }
}}

class MessagePackValueObject(val vo : ValueObject) extends MessagePackObject {
  def messagePack(packer : Packer) {
    require(packer.isInstanceOf[VOPacker]);
    packer.asInstanceOf[VOPacker].pack(vo);
  }
}

case class SpecialValueType(bytes : Int, factory: (Array[Byte], Int) => MessagePackObject)
