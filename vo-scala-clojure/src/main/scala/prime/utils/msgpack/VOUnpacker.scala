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

  final def processValueType(typeID: Int) = typeID match {
    case 0x1D => 12
  }

  final def putValue(bytes: Array[Byte], startIndex: Int): Unit = data = bytes(startIndex) match {
    case 0x1D => new MessagePackObjectId( new ObjectId(bytes.slice(startIndex + 1, startIndex + 13)) )
  }

  // ---
  // Value Objects
  protected var values      : scala.collection.mutable.Builder[(Int, Any),scala.collection.immutable.Map[Int,Any]] = _
  protected var mainType    : Int = -1;
  protected var typeID_sl8  : Int =  0;
  protected var bits8       : Int =  0;
  protected var fieldOffset : Int =  0;

  final def prepareValueObject(typeID: Int) {
    if (values == null) {
      //println("\n - New VO: " + typeID);
      values   = scala.collection.immutable.Map.newBuilder[Int,Any];
      mainType = typeID;
    }

    typeID_sl8 = typeID << 8;
    fieldOffset = 0;
  }

  final def putValue(value: AnyRef) {
    val i   = numberOfTrailingZeros(bits8);
    values += ((typeID_sl8 | (fieldOffset + i), value));  //println(v._1.formatted("0x%x"), v._2);
    bits8  ^= 1 << i;
    if (bits8 == 0) fieldOffset += 8;
  }

  final def fieldgroupRequiresMoreValues = bits8 != 0

  final def prepareForNext8Fields(fields: Byte) {
    this.bits8 = fields & 0xFF;
  }

  final def getData = {
    val d = if (data != null) data else {
      //println("  -  Finished VO: " + mainType + "\n");
      new MessagePackValueSource(mainType, values.result);
    }
    data = null; values = null; mainType = -1;
    d
  }
}

class MessagePackValueSource(val typeID : Int, val map : scala.collection.Map[Int,_]) extends MessagePackObject with ValueSource with NoPrimitives {
  def contains (ignored: String, idx : Int)                : Boolean = map.contains(idx >>> 8);
  def anyAt    (ignored: String, idx : Int, notFound: Any) : Any     = map.get(idx >>> 8) getOrElse notFound;

  override def toString = getClass.getSimpleName +"("+ map +")";
  override def typeID(baseTypeID:Int) = typeID;
  def messagePack(packer : Packer) = throw new UnsupportedOperationException("why would I?");
}
