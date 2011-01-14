package primevc.core.traits;
 import com.mongodb.casbah.Imports._
 import primevc.types._
import primevc.utils.msgpack.VOPacker

trait ObjectId {
  type IDType = org.bson.types.ObjectId;
  
  /*@field*/ protected[this] var $id: org.bson.types.ObjectId = null
  def id : org.bson.types.ObjectId = $id;
  def id_=(v : org.bson.types.ObjectId);
  def id_ (v : AnyRef);
}

object ObjectIdVO {
  val id = Field('id, Type.TuniqueID)
}

object ObjectId {
	def msgpack_packVO(o : VOPacker, obj : ObjectId, flagsToPack : Int)
	{
		require(o != null && obj != null && flagsToPack != 0);

		o.packValueObjectHeader(0x1D, 0, 1);
		o.writeByte(1);
		o.pack(obj);
	}
}
