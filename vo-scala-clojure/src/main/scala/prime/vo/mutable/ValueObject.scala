package prime.vo.mutable;
 import prime.types._
 import scala.collection.JavaConversions
 import org.msgpack.Unpacker
 import prime.utils.msgpack.{MutableVOPacker, MutableVOUnpacker, VOInstanceUnpacker}

trait ValueObject extends java.io.Externalizable
{
  override def toString = getClass.getSimpleName + "(" + (voCompanion.fields.filter(f => this.fieldIsSet_?(f.name)).map {
    f => f.name.name + " = " + voCompanion.getValue(this, f.name.name)
  } mkString(", ")) + ")";

  protected[prime] def updateFieldsSet_!() { }

  protected[prime] def voCompanion : VOCompanion[_] with VOMessagePacker[_]
  protected[prime] var $fieldsSet : Int = 0

  def fieldIsSet_?(field:Field): Boolean = {
    updateFieldsSet_!
    fieldIsSet_?(voCompanion.field(field.name.name))
  }
  def fieldIsSet_?(name:Symbol): Boolean = fieldIsSet_?(voCompanion.field(name))
  
  /** Which field (as defined by the companion object fields Vector indices) is set? */
  def fieldIsSet_?(index:Int): Boolean = ($fieldsSet & (1 << index)) != 0
  def empty_? = { updateFieldsSet_! ; $fieldsSet == 0 }

  def partial_? : Boolean = numFieldsSet_? != voCompanion.numFields
  def validationErrors_? : List[(Symbol, String)] = Nil

  // Courtesy of: http://graphics.stanford.edu/%7Eseander/bithacks.html#CountBitsSetParallel
  def numFieldsSet_? : Int = {
    updateFieldsSet_!
    var v = $fieldsSet
    v = v - ((v >> 1) & 0x55555555);                    // reuse input as temporary
    v = (v & 0x33333333) + ((v >> 2) & 0x33333333);     // temp
    v = ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24; // count
    v
  }

  def readExternal(in : java.io.ObjectInput)
  {
    val obj = this;
    val map = obj.voCompanion.defaultVOCompanionMap;
    assert(map != null, this.getClass+"::readExternal map == null, obj = " + obj)

    val helper = new MutableVOUnpacker(map)
    {
      var first = true;
      override def newObject = if (!first) super.newObject else {
        first = false;
        new VOInstanceUnpacker(map) {
          vo  = obj
          voc = obj.voCompanion.asInstanceOf[VOCompanion[ValueObject]]
          assert(voc != null, this.getClass+"::readExternal voc == null, obj = "+obj)
        }
      }
    }
    val unpacker = new Unpacker(in.asInstanceOf[java.io.ObjectInputStream])
    unpacker.setVOHelper(helper)
    
    try {
      unpacker.next
    } catch {
      case e =>
        e.printStackTrace();
        throw new java.lang.Exception(this+"::readExternal of '" + this.getClass + "' failed: " + e)
    }
  }

  def writeExternal(out : java.io.ObjectOutput) {
    new MutableVOPacker(out.asInstanceOf[java.io.ObjectOutputStream]).pack(this)
  }
}

trait ValueObjectWithID extends ValueObject
{
  type IDType <: Any

  /** The original ID of this object */
  val _id: IDType;
}

abstract class AbstractValueObject() extends ValueObject {}


trait VOAccessor[V <: ValueObject]
{
  // abstract
  def field(vo:V, index: Int): Field
  def field(vo:V, key:String): Int
  def fieldsFor(vo:V): IndexedSeq[Field]
  def getValue(vo:V, key:String): AnyRef
  def putValue(vo:V, key:String, value:AnyRef): V

  // concrete
  final def fieldNamed(vo:V, key:String) = field(vo, field(vo, key))
  final def getValue  (vo:V, key:Symbol): AnyRef = getValue(vo,key)
  final def putValue  (vo:V, key:Symbol, value:AnyRef): V = putValue(vo,key.name,value)

  def isSet(vo:V, key:String): Boolean = vo.fieldIsSet_?(field(vo, key));

  /** Is this VO partially set?
      In other words: should a proxy overwrite or merge this VO with another that has the same ID ? */
  def partial_?(vo:V) = vo.numFieldsSet_? < fieldsFor(vo).size

  def fieldsSetNames(vo:V) = fieldsFor(vo).indices.view filter(vo.fieldIsSet_?(_)) map(field(vo, _).name.name) toSet
}

trait VOFieldInfo
{
  val fields: Array[Field] = Array()
  def numFields: Int = fields.length
  def field(index: Int): Field = throw new MatchError("Field with index "+index+" not found in this VO");
  def field(key:String): Int = -1

  @inline final def field(key: Symbol): Field = fieldNamed(key.name)
  @inline final def fieldNamed(key:String): Field = field(field(key))
}

trait VOMessagePacker[V <: ValueObject] {
  protected[prime] def msgpack_packVO(o : MutableVOPacker, obj : V, flagsToPack : Int) : Unit
}

trait VOCompanion[V <: ValueObject] extends VOAccessor[V] with VOFieldInfo {
  type VOType = V
  val TypeID : Int

  @inline final def field(vo:VOType, key:String): Int   = field(key)
  @inline final def field(vo:VOType, key:Symbol): Field = fieldNamed(key.name)
  @inline final def field(vo:VOType, index: Int): Field = field(index)

  def fieldsFor(vo:VOType): IndexedSeq[Field] = this.fields

  def getValue(vo:VOType, key:String): AnyRef = field(key) match {
    case index:Int => getValue(vo, index)
    case _ => null
  }

  def putValue(vo:VOType, key:String, value:AnyRef): VOType = field(key) match {
    case -1 => vo
    case index:Int => putValue(vo, index, value)
  }

//  def clear(vo:VOType) = for (i <- 0 until numFields) putValue(vo, i, null)

  def getValue(vo:VOType, index:Int): AnyRef
  def putValue(vo:VOType, index:Int, value:AnyRef): VOType
  def empty: VOType
  def fieldIndexOffset(typeID : Int) : Int

  def defaultVOCompanionMap : scala.collection.immutable.IntMap[VOCompanion[_]]
}

trait IDAccessor[V <: ValueObjectWithID]
{
//  val idField: Field
  def idValue (vo:V): V#IDType
  def idValue (vo:V, idValueToSet:V#IDType): Unit
}

trait VOProxy[V <: ValueObjectWithID] extends IDAccessor[V]// with VOAccessor[V] with VOFieldInfo
{
  def findByID(id: V#IDType) : Option[V]
}

trait VOProxyComponent
{
  protected def outer: this.type = this
/*
  def putValue(vo: ValueObject, key: String, value: AnyRef): ValueObject =
    this.getClass.getMethod("putValue", vo.getClass,  classOf[String], classOf[Any]).invoke(this, vo, key, value.asInstanceOf[AnyRef]).asInstanceOf[ValueObject]

  def getValue(vo: ValueObject, key: String): AnyRef = {
    this.getClass.
    this.getClass.getMethod("getValue", vo.getClass, classOf[String]).invoke(this, vo, key)
  }

  def fieldsFor(vo: ValueObject): IndexedSeq[Field] =
    this.getClass.getMethod("fieldsFor", vo.getClass).invoke(this, vo).asInstanceOf[IndexedSeq[Field]]

  def field(vo: ValueObject, index: Int): Field =
    this.getClass.getMethod("field", vo.getClass, classOf[Int]).invoke(this, vo, index.asInstanceOf[AnyRef]).asInstanceOf[Field]

  def field(vo: ValueObject, key: String): Int =
    this.getClass.getMethod("field", vo.getClass, classOf[String]).invoke(this, vo, key).asInstanceOf[Int]
*/
}

trait XMLConverter[V <: ValueObject] //extends VOAccessor[V]
{
  def toXML(vo:V): scala.xml.NodeSeq
  def setValueObject(vo:V, xml: scala.xml.NodeSeq) : Option[V]
}

trait XMLProxy[V <: ValueObjectWithID] extends XMLConverter[V] with VOProxy[V]

trait XMLComponent extends VOProxyComponent with XMLConverter[ValueObject]
{
  def setValueObject(vo:ValueObject, xml: scala.xml.NodeSeq) : Option[ValueObject] =
    this.getClass.getMethod("setValueObject", vo.getClass, classOf[scala.xml.NodeSeq]).invoke(this, vo, xml).asInstanceOf[Option[ValueObject]]

  def toXML(vo:ValueObject): scala.xml.NodeSeq =
    this.getClass.getMethod("toXML", vo.getClass).invoke(this, vo).asInstanceOf[scala.xml.NodeSeq]
}
