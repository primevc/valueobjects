package primevc.core.traits;
 import com.mongodb.casbah.Imports._
 import primevc.types._

trait ObjectId {
  type IDType = org.bson.types.ObjectId;
  
  def id : org.bson.types.ObjectId;
  def id_=(v : org.bson.types.ObjectId);
  def id_=(v : AnyRef);
}

object ObjectIdVO {
  val id = Field('id, Type.TuniqueID)
}
