package prime.vo.util;
 import prime.vo._
 import scala.collection.immutable.IntMap;
 import com.esotericsoftware.kryo._

object KryoSerializer
{
	def decorate(kryo : Kryo, typeMap : IntMap[ValueObjectCompanion[_ <: ValueObject]]) {
		typeMap.foreachValue { voCompanion =>
      val voClass = voCompanion.empty.getClass;
      kryo.register(voClass, new KryoSerializer(voCompanion), voCompanion.manifest.ID + 100);
    }
	}
}

final class KryoSerializer[VOType <: ValueObject](voCompanion : ValueObjectCompanion[VOType]) extends Serializer[VOType](true, true)
{
  import java.lang.Integer._

  def write (kryo:Kryo, out:io.Output, vo:VOType) {
    out.writeInt(vo.voIndexSet);
    //println("write vo `" + voCompanion.manifest.VOType.erasure.getName + "` voIndexSet = " + vo.voIndexSet);
    vo.foreach { (f: ValueObjectField[vo.VOType], value: Any) =>
      //println("write field `" + f.name + "` = " + value);
      kryo.writeClassAndObject(out, value);
    }
  }

  def create(kryo:Kryo, in: io.Input, clz:Class[VOType]): VOType = {
    voCompanion.empty
  }

  def read  (kryo:Kryo, in: io.Input, clz:Class[VOType]): VOType = {
    val fieldSet = in.readInt();
    if (fieldSet != 0)
    {
      val data = new Array[Any](numberOfTrailingZeros(highestOneBit(fieldSet)) + 1);
      //println("data.length = "+ data.length);
      var i    = numberOfTrailingZeros(fieldSet);
      var bits = fieldSet >>> i;
      do {
        val value = kryo.readClassAndObject(in);
        data(i) = value;
        //println("data["+i+"] = " + value);
        bits >>>= 1;
        val inc = numberOfTrailingZeros(bits);
        bits >>>= inc;
        i      += 1 + inc;
      }
      while (bits != 0);

      val src = new source.AnonymousValueSource(data :_*);
      //println(src);
      voCompanion(src);
    }
    else
      voCompanion.empty;
  }
}
