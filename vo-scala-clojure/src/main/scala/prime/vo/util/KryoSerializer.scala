package prime.vo.util;
 import prime.vo._
 import scala.collection.immutable.IntMap;
 import com.esotericsoftware.kryo._

object KryoSerializer
{
	def decorate(kryo : Kryo, typeMap : IntMap[ValueObjectCompanion[_ <: ValueObject]]) {
		typeMap.foreachValue { voCompanion =>
      val voClass = voCompanion.empty.conj(source.ValueSource.empty).getClass;
      kryo.register(voClass, new KryoSerializer(voCompanion), voCompanion.manifest.ID + 100);
    }
	}
}

/**
  Format:
  || 5-bits            | 3-bits (0-4)     || n bytes    || Per self-type field ||( Bit: id         | Bit: f     [f-bit = 0:| 6-Bits   ]or[f-bit = 1:| 3-Bits (0-4) || 1-2 bytes || n bytes   ]|| Per field...  ) Per mixin...
  || Number-of-mixins  | Self-field bytes || Field bits || Data...             ||( +1 TypeID byte? | >6 fields? [          | Field 0-5]or[                         || Type ID   || Field bits]|| Data          ) ...         
*/
final class KryoSerializer[VOType <: ValueObject](voCompanion : ValueObjectCompanion[VOType]) extends Serializer[VOType](true, true)
{
  import prime.utils.msgpack.Util._
  import java.lang.Integer._

  def create(kryo:Kryo, in: io.Input, clz:Class[VOType]): VOType = {
    voCompanion.empty
  }

  def writeBytes(bits : Int, bytes : Int, out : io.Output): Unit = bytes match {
    case 0 => print("Write nothing; ");
    case 1 => print("Write 1: "+ toBinaryString(bits) +"; "); out.writeByte (bits)
    case 2 => print("Write 2: "+ toBinaryString(bits) +"; "); out.writeShort(bits)
    case 3 => print("Write 3: "+ toBinaryString(bits) +"; "); out.writeByte (bits >>> 16); out.writeShort(bits);
    case 4 => print("Write 4: "+ toBinaryString(bits) +"; "); out.writeInt  (bits)
  }
  def readBytes(bytes : Int, in : io.Input): Int = bytes match {
    case 0 => print("Read 0; "); 0;
    case 1 => val bits = in.readByte()
              print("Read 1: "+ toBinaryString(bits) +"; "); bits;
    case 2 => val bits = in.readShort()
              print("Read 2: "+ toBinaryString(bits) +"; "); bits;
    case 3 => val bits = (in.readByte() << 16) | in.readShort();
              print("Read 3: "+ toBinaryString(bits) +"; "); bits;
    case 4 => val bits = in.readInt()
              print("Read 4: "+ toBinaryString(bits) +"; "); bits;
  }
  def writeFields(kryo:Kryo, out:io.Output, vo:VOType, mixin: ValueObjectMixin[_]) = vo.foreach(mixin.fieldIndexMask) { 
    (f: ValueObjectField[vo.VOType], value: Any) =>
      print("write value: " + value + "; ");
      kryo.writeClassAndObject(out, value match {
        case v : URI       => v.getEscapedURIReference
        case v : EmailAddr => v.toString
        case e : EnumValue => if (!classOf[scala.Product].isInstance(e)) e.value else e.toString;
        case v => v
      });
  }

  def write (kryo:Kryo, out:io.Output, vo:VOType) {
    val fieldSet  = vo.voIndexSet;
    if (fieldSet != 0)
    {
      val mixin     =  voCompanion.manifest.metaMixin;
      val mixinBits = mixin.indexBitsShifted(fieldSet);
      val bits8     = (voCompanion.manifest.mixinCount(fieldSet) << 3) | bytesUsedInInt(mixinBits);
      // Self header
      print("\nHeader " + toBinaryString(bits8) +": ");
      out.writeByte(bits8);
      if ((bits8 & 7) != 0) {
        // Self field index
        writeBytes(mixinBits, bits8 & 7, out);
        // Self data
        writeFields(kryo, out, vo, mixin);
      }

      // Per mixin header + typeID + data
      if ((fieldSet & mixin.fieldIndexMask) != fieldSet) for (mixin <- voCompanion.manifest.mixins)
      {
        val mixinBits = mixin.indexBitsShifted(fieldSet);
        print("\n| mixin["+ mixin.manifest.ID +"] bits: " + mixinBits + "; ");
        if (mixinBits != 0)
        {
          // header
          val bits8 = (if (mixin.manifest.ID < 0xFF) 0 else 0x80) | (if ((mixinBits & 0x3F) == mixinBits) mixinBits else 0x40 | bytesUsedInInt(mixinBits));
          out.writeByte(bits8);
          print("  header " + toBinaryString(bits8) +": ");
          // typeID
          if ((bits8 & 0x80) == 0) out.writeByte(mixin.manifest.ID) else out.writeShort(mixin.manifest.ID);
          // field index
          if ((bits8 & 0x40) != 0) writeBytes(mixinBits, bits8 & 7, out);
          // data
          writeFields(kryo, out, vo, mixin);
        }
      }
      println(".");
    }
    else
      out.writeByte(0);
  }

  def readFields(kryo:Kryo, in: io.Input, typeID_sl8:Int, fieldSet:Int, data:scala.collection.mutable.Builder[(Int, Any),scala.collection.immutable.Map[Int,Any]]) = if (fieldSet != 0) {
    var i    = numberOfTrailingZeros(fieldSet);
    var bits = fieldSet >>> i;
    do {
      val value = kryo.readClassAndObject(in);
      data += ((typeID_sl8 | i, value));
      // calc next field ID
      bits >>>= 1;
      val inc = numberOfTrailingZeros(bits);
      bits >>>= inc;
      i      += 1 + inc;
    }
    while (bits != 0);
  }

  def read  (kryo:Kryo, in: io.Input, clz:Class[VOType]): VOType = {
    val header  = in.readByte();
    print("\nHeader " + toBinaryString(header) +": ");
    if (header != 0)
    {
      val data = scala.collection.immutable.Map.newBuilder[Int,Any];
      val fields = readBytes(header & 7, in);
      readFields(kryo, in, voCompanion.manifest.ID << 8, fields, data);

      var mixinCount = header >>> 3;
      while (mixinCount > 0) {
        val header = in.readByte();
        val typeID = if ((header & 0x80) == 0) in.readByte() else in.readShort();
        val fields = if ((header & 0x40) == 0) header & 0x3F else readBytes(header & 7, in);
        print("\n| mixin header["+ toBinaryString(header) +"] type: "+ typeID +" bits: " + fields + "; ");
        readFields(kryo, in, typeID << 8, fields, data);
        mixinCount -= 1;
      }
      val src = new source.ScalaMapValueSource_Int(data.result);
      println(src);
      voCompanion(src);
    }
    else
      voCompanion.empty;
  }
}
