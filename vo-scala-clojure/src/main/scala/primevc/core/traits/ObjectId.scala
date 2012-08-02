package primevc.core.traits;
 import com.mongodb.casbah.Imports._
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

object ObjectId {
	object manifest extends ValueObjectManifest_1[ObjectId] {
    val ID = 0x1D;
    object id extends ValueObjectField[ObjectId](0x1D00,'id,TuniqueID,null) {
      def apply(vo : ObjectId) = vo.id;
    }
    val first = id;
  }
}
