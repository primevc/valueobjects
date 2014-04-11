package prime.utils.msgpack
 import prime.vo._
 import prime.vo.source._
 import org.msgpack.{Packer, MessagePackObject, UnpackerImpl}
 import org.bson.types.ObjectId
 import java.lang.Integer._

object VOUnpacker extends UnpackerImpl.VOHelper {
  def newObject = new VOInstanceUnpacker
}

final class VOInstanceUnpacker extends UnpackerImpl.VOInstance
{
  var data : MessagePackObject = _

  // ---
  // Value Types

  final def processValueType(typeID: Int, bytes: Array[Byte]) = typeID match {
    case 0x1D => 12
    /*
    case 0x0F => 32
    */
  }

  final def putValue(bytes: Array[Byte] /*, startIndex: Int*/): Unit = data = bytes(startIndex) match {
    case 0x1D => new MessagePackObjectId( new ObjectId(bytes.slice(startIndex + 1, startIndex + 13)) )
    /*
    case 0x0F => new MessagePackFileRef( new FileRef(bytes.slice(startIndex + 1, startIndex + 33)) )
    */
  }

  // ---
  // Value Objects
  protected var indices     : collection.mutable.ArrayBuilder.ofInt = _
  protected var values      : scala.collection.mutable.ArrayBuilder[Any] = _
  protected var mainType    : Int = -1;
  protected var typeID_sl8  : Int =  0;
  protected var bits8       : Int =  0;
  protected var fieldOffset : Int =  0;

  final def prepareValueObject(typeID: Int) {
    if (values == null) {
      //println("\n - New VO: " + typeID);
      indices  = new collection.mutable.ArrayBuilder.ofInt(); indices.sizeHint(32);
      values   = collection.mutable.ArrayBuilder.make();       values.sizeHint(32);
      mainType = typeID;
    }

    typeID_sl8  = typeID << 8;
    fieldOffset = 0;
  }

  final def putValue(value: AnyRef) {
    val i    = numberOfTrailingZeros(bits8);
    indices += typeID_sl8 | (fieldOffset + i);
    values  += value;
    bits8   ^= 1 << i;
    if (bits8 == 0) fieldOffset += 8;
  }

  final def fieldgroupRequiresMoreValues = bits8 != 0

  final def prepareForNext8Fields(fields: Byte) {
    if (fields == 0) fieldOffset += 8;
    this.bits8 = fields & 0xFF;
  }

  final def getData = {
    val d = if (data != null) data else {
      //println("  -  Finished VO: " + mainType + "\n");
      new MessagePackValueSource(mainType, indices.result, values.result);
    }
    data = null; indices = null; values = null; mainType = -1;
    d
  }
}

class MessagePackValueSource(val typeID : Int, val ids : Array[Int], val values : Array[Any]) extends MessagePackObject with IntAnyArrayValueSource {
  def messagePack(packer : Packer) = throw new UnsupportedOperationException("why would I?");
}
