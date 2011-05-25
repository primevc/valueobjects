package primevc.mvc.mongodb
 import primevc.core.traits._
 import primevc.types._
 import org.bson.BSONObject
 import org.joda.time.Interval
 import scala.collection.JavaConverters._
 import scala.collection.mutable.ListBuffer
 import com.mongodb.casbah.Imports._
import primevc.utils.ConvertTo

class IntervalDBObject(val interval:Interval) extends DBObject with BSONObject
{
  def get(key:String) : AnyRef = key match {
    case "s" => interval.getStart.toDate
    case "e" => interval.getEnd.toDate
    case _ => null
  }

  def putAll(map: java.util.Map[_,_]) = unimplemented
  def putAll(obj: BSONObject) = unimplemented
  def put(key:String, value:AnyRef) = unimplemented
  def markAsPartialObject(): Unit = unimplemented
  def removeField(name: String) = unimplemented
  def toMap = unimplemented

  def isPartialObject() = false

  def keySet : java.util.Set[String] = _keySet

  def containsField(name: String) = name == "s" || name == "e"
  def containsKey(k: String) = containsField(k);

  private object _keySet extends java.util.TreeSet[String] {
    add("s")
    add("e")
  }

  private def unimplemented = throw new Exception("not supported")
}

abstract class DBObjectVO[V <: ValueObject] (val vo:V) //(implicit val converter:MongoConverter)
 extends ValueObject with DBObject with BSONObject
{
  require(vo != null)

  def Companion = vo.Companion

  var partial = false;
  var typeCast = false;

  def withTypeCast(): this.type = {
    typeCast = true;
    this;
  }

  /** Gets a field from this object by a given name.
   * @param key The name of the field fetch
   * @return The field, if found
   */
  def get(key:String) : AnyRef

  /** Sets a name/value pair in this object.
   * @param key Name to set
   * @param v Corresponding value
   * @return <tt>v</tt>
   */
  def put(key:String, value:AnyRef) : AnyRef //= converter.putValue(vo, key, value)

  /**
   * if this object was loaded with only some fields (using a field filter)
   * this method will be called to notify
   */
  def markAsPartialObject(): Unit = partial = true;

  /**
   * whether markAsPartialObject was ever called
   * only matters if you are going to upsert and dont' want to risk losing fields
   */
  def isPartialObject() = partial // || converter.partial_?(vo)

  /** Returns this object's fields' names
   * @return The names of the fields in this object
   */
  def keySet: java.util.Set[String]

  def keySet(fields : IndexedSeq[Field], field : Int => Field): java.util.Set[String] = {
    val fieldNames : Iterator[String] = fields.indices.iterator.filter(vo.fieldIsSet_?(_)).map(field(_).name.name)
    (if (typeCast) Iterator("cast") ++ fieldNames else fieldNames).toSet.asJava
  }

  /** Checks if this object contains a field with the given name.
   * @param s Field name for which to check
   * @return if this object contains a field with the given name
   */
  def containsField(name: String): Boolean //= converter.isSet(vo, name)

  def containsKey(k: String) = containsField(k);

  /** Remove a field with a given name from this object.
   * @param key The name of the field to remove
   * @return The value removed from this object
   */
  def removeField(name: String) = put(name, null);

  /**
   * Returns a map representing this BSONObject.
   * @return the map
   */
  def toMap() = {
    val map = new java.util.TreeMap[String, AnyRef]
    for (key <- keySet.asScala) map.put(key, this.get(key))
    map
  } //throw new Exception("Moet dat echt?");

  override def toString = toMap.toString

  def putAll(map: java.util.Map[_,_]) {
    val m = map.asInstanceOf[java.util.Map[String,AnyRef]]
    for (k <- m.keySet().iterator().asScala) {
      try {
        put(k, m.get(k))
      } catch {
        case e:MatchError =>
      }
    }
  }

  def putAll(obj: BSONObject) {
    for (k <- obj.keySet.asScala) {
      try {
        put(k, obj.get(k))
      } catch {
        case e:MatchError =>
      }
    }
  }

  override def numFieldsSet_? : Int = vo.numFieldsSet_?

  /** Which field (as defined by the companion object fields Vector indices) is set? */
  override def fieldIsSet_?(index:Int): Boolean = vo.fieldIsSet_?(index)

  override def partial_? : Boolean = isPartialObject
  override def empty_? : Boolean = vo.empty_?
}


// --------------
// -- Proxying --
// --------------

trait MongoConverter //extends VOAccessor[ValueObject]
{
/*  def asDBObject(vo:ValueObject): DBObject
  def setValueObject(vo:ValueObject, dbo: MongoDBObject) : ValueObject
  def fieldsFor(vo:ValueObject) : IndexedSeq[Field]
*/
}

trait MongoComponent extends VOProxyComponent with MongoConverter
{
/*
  def asDBObject(vo:ValueObject): DBObject =
    this.getClass.getMethod("asDBObject", vo.getClass).invoke(this, vo).asInstanceOf[DBObject]

  def setValueObject(vo:ValueObject, dbo:MongoDBObject): ValueObject =
    this.getClass.getMethod("setValueObject", vo.getClass, classOf[MongoDBObject]).invoke(this, vo, dbo).asInstanceOf[ValueObject]
*/
}

trait ReliableSave[V <: ValueObjectWithID]
{
  this: MongoProxy[V] =>

  def save(p: V, idReadFromDB: V#IDType) = saveReliably(p, idReadFromDB)

  def saveReliably(p: V, idReadFromDB: V#IDType) = {
    db.requestStart

    saveFast(p, idReadFromDB)

    val x = db.command(GetLastError.msg)
    val result = x get "err" match {
      case null => Some(x)
      case msg: String => None
    }

    db.requestDone
    result
  }
}

private object GetLastError {
  val msg = MongoDBObject("getlasterror" -> "1", "w" -> "2")
}

abstract class MongoProxy[V <: ValueObjectWithID](implicit voManifest:Manifest[V]) extends VOProxy[V]
{
  implicit protected def s2db(name: String): MongoDB = conn(name)
  implicit protected def s2coll(name: String): MongoCollection = db(name)

  var conn: MongoConnection = _
  var db  : MongoDB         = _
  var coll: MongoCollection = _

  def save(valueobject: V, idReadFromDB: V#IDType = null.asInstanceOf[V#IDType]) = saveFast(valueobject, idReadFromDB)

  /**
   * Upserts/saves the valueobject with _ID_.
   * Let _ID_ = idReadFromDB, or if this parameter is null: the ID-value stored in the valueobject.
   *
   * Tries to do the most sane thing made possible by MongoDB:
   *
   * Partial upserts are not allowed (by MongoDB) to change the _id property.
   * If _ID_ is not null and valueobject is partial:
   *  - _id is unset
   *  - a $set update is performed where _id = _ID_
   *
   * If  _ID_ is null, partial_? is set to false, and a save() is performed.
   *
   * Finally, if _ID_ is not null: _id is set to _ID_
   */
  def saveFast(valueobject: V, idReadFromDB: V#IDType = null.asInstanceOf[V#IDType], possiblyPartial: Boolean = false) =
  {
    val id = if (idReadFromDB != null) idReadFromDB else idValue(valueobject);
    val dbo = asDBObject(valueobject);

    if (possiblyPartial && id != null && valueobject.partial_?)
    {
      println("- PARTIAL UPDATE["+id+"]: "+asDBObject(valueobject));
      idValue(valueobject, null.asInstanceOf[V#IDType]); // make sure ID is null for the update command to work...
      coll.update(MongoDBObject("_id" -> id), MongoDBObject("$set" -> asDBObject(valueobject)), true, false);
    }
    else
    {
      val dbo = asDBObject(valueobject);
//      dbo.markAsPartialObject();
      idValue(valueobject, id)
      println("- SAVE: "+dbo);
      coll.underlying.save(dbo)
    }
    if (id != null) idValue(valueobject, id);
  }

  def delete(p: V, idReadFromDB: V#IDType) =
  {
    val id = if (idReadFromDB != null) idReadFromDB else idValue(p);
    println("Mongo DELETE: "+id);
    coll.remove(MongoDBObject("_id" -> id))
  }

  def deleteAll():Unit = coll.remove(MongoDBObject.empty);

  implicit val manifest_IDType: Manifest[V#IDType] = implicitly
  def idType() : Class[_] = manifest_IDType.erasure

  def findByID(id: V#IDType): Option[V] = coll.findOne(MongoDBObject("_id" -> id)).map(asObject(_))

  def find() = coll.find.map(asObject(_))

/*
  def fetch(offset:Int = 0, length:Int = 100, orderBy:String = null, orderDirection:String = null, keywords:String = null) = {
    println("Mongo fetch() offset:"+offset+", length:"+length+", orderBy:"+orderBy+", orderDirection:"+orderDirection+", keywords:"+keywords);
    val qry:MongoCursor = if (keywords != null) execFullTextSearch(keywords) else this.coll.find(MongoDBObject.empty, MongoDBObject.empty, offset, length)

    // Use collection views?
    qry.map(asObject(_))
  }

  def count(keywords:String = null) = {
    println("Mongo count() keywords:"+keywords);
    if (keywords != null) execFullTextSearch(keywords).length
    else coll.count
  }


  def fullTextSearch(keywords:String, offset:Int=0, length:Int=0) = execFullTextSearch(keywords,offset,length).map(asObject(_)) // Collection views?

  protected def execFullTextSearch(keywords:String, offset:Int=0, length:Int=0) = {
    val regex = keywords.r
    val props = allProps.filter(_.outerType == classOf[String]).map(p => MongoDBObject((if (p.id_?) "_id" else p.name) -> regex))
    val qry = MongoDBObject("$or" -> props.toArray)
    if (offset + length > 0)
      coll.find(qry, MongoDBObject.empty, offset, length)
    else
      coll.find(qry)
  }
*/

//  override def asDBObject(valueobject:V) : DBObject = new DBObjectVO(valueobject)(this);

  // --- Abstracts ---
  def asDBObject(valueobject:V) : DBObject
  def asObject(dbo:MongoDBObject) : V
//  def getValue(obj:V, key:String) : AnyRef
//  def putValue(obj:V, key:String, value:AnyRef) : V
}

/* WIP:


abstract class MongoProxy[V <: ValueObjectWithID] extends VOProxy[V]
{
  // --- Abstracts ---
  def asDBObject(valueobject:V) : DBObject
  def asObject(dbo:MongoDBObject) : V
  def getValue(obj:V, key:String) : AnyRef
  def putValue(obj:V, key:String, value:AnyRef) : V

  // --- Concretes ---
  def find(query: (Field, Any)*) = {
    val dbo = new BasicDBObject
    for (kv <- query) dbo.append(kv._1.name.name, kv._2)

    find(dbo)
  }

  def find(vo:V) = find(asDBObject(vo))

  def find(query:DBObject) = {}

  def save(valueobject: V, idReadFromDB: V#IDType = null.asInstanceOf[V#IDType]) = saveFast(valueobject, idReadFromDB)

  /**
   * Upserts/saves the valueobject with _ID_.
   * Let _ID_ = idReadFromDB, or if this parameter is null: the ID-value stored in the valueobject.
   *
   * Tries to do the most sane thing made possible by MongoDB:
   *
   * Partial upserts are not allowed (by MongoDB) to change the _id property.
   * If _ID_ is not null and valueobject is partial:
   *  - _id is unset
   *  - a $set update is performed where _id = _ID_
   *
   * If  _ID_ is null, partial_? is set to false, and a save() is performed.
   *
   * Finally, if _ID_ is not null: _id is set to _ID_
   */
  def saveFast(valueobject: V, idReadFromDB: V#IDType = null.asInstanceOf[V#IDType]) =
  {
    val id = if (idReadFromDB != null) idReadFromDB else idValue(valueobject);
    val dbo = asDBObject(valueobject);

    if (id != null && partial_?(valueobject))
    {
      println("- PARTIAL UPDATE["+id+"]: "+asDBObject(valueobject));
      idValue(valueobject, null.asInstanceOf[V#IDType]); // make sure ID is null for the update command to work...
      coll.update(MongoDBObject("_id" -> id), MongoDBObject("$set" -> asDBObject(valueobject)), true, false);
    }
    else
    {
      val dbo = asDBObject(valueobject);
//      dbo.markAsPartialObject();
      println("- SAVE: "+dbo);
      coll.underlying.save(dbo)
    }
    if (id != null) idValue(valueobject, id);
  }

  def delete(p: V, idReadFromDB: V#IDType) =
  {
    val id = if (idReadFromDB != null) idReadFromDB else idValue(p);
    println("Mongo DELETE: "+id);
    coll.remove(MongoDBObject("_id" -> id))
  }

  def deleteAll():Unit = coll.remove(MongoDBObject.empty);
}

*/
