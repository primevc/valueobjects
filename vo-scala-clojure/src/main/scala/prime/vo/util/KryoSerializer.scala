package prime.vo.util;
 import prime.vo._
 import scala.collection.immutable.IntMap;

object KryoSerializer
{
	def decorate(kryo : Kryo, typeMap : IntMap[ValueObjectCompanion[_]]) {
		kryo.addDefaultSerializer(classOf[ValueObject], this);
		val serializer = new KryoSerializer(typeMap);

    typeMap.foreachValue { voCompanion =>
      val voClass = voCompanion.empty.getClass;
      kryo.register(voClass, new KryoSerializer(voCompanion), voCompanion.manifest.ID + 100);
    }
	}
}

final class KryoSerializer[VOType <: ValueObject](voCompanion : ValueObjectCompanion[VOType])
{
  def write (kryo:com.esotericsoftware.kryo.Kryo, out:com.esotericsoftware.kryo.io.Output, vo:VOType) {
    
  }

  def create(kryo:com.esotericsoftware.kryo.Kryo, in: com.esotericsoftware.kryo.io.Input, clz:Class[VOType]): VOType = {
    
  }

  def read  (kryo:com.esotericsoftware.kryo.Kryo, in: com.esotericsoftware.kryo.io.Input, clz:Class[VOType]): VOType = {
    
  }
}
