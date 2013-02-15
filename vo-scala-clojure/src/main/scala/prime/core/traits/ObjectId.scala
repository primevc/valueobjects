package prime.core.traits;
 import prime.types._
 import prime.types.ValueTypes._
 import prime.utils.msgpack.MutableVOPacker
 import prime.vo._

package mutable
{
  trait ObjectId extends prime.vo.mutable.ValueObjectWithID {
    type IDType = prime.types.ObjectId;

    protected[this] var __id: org.bson.types.ObjectId = null
    def id : prime.types.ObjectId = __id;
    def id_=(v : org.bson.types.ObjectId);
    def id_ (v : AnyRef);
  }

  object ObjectId {
    def msgpack_packVO(o : MutableVOPacker, obj : mutable.ObjectId, flagsToPack : Int)
    {
      require(o != null && obj != null && flagsToPack != 0);

      o.packValueObjectHeader(0x1D, 0, 1);
      o.writeByte(1);
      o.pack(obj.id);
    }
  }

  object ObjectIdVO {
    val id = Field('id, Type.TuniqueID)
  }
}

trait ObjectId extends ValueObject with ID {
  type IDType = prime.types.ObjectId;
  val  id     : prime.types.ObjectId;
  def _id = id;
}

object ObjectId
{
	object field {
    object id extends ValueObjectField[ObjectId](0x1D00,'id,TobjectId,null) {
      def apply(vo : ObjectId) = vo.id;
    }
  }

  object manifest extends {
    val ID     =      0x1D;
    val id     =  field.id;
    val first  =  field.id;
    val lastFieldIndex = 0;
    val mixins = Array[ValueObjectMixin]();
  } with ValueObjectManifest_1[ObjectId];
}
