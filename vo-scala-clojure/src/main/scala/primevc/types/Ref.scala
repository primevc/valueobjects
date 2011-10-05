package primevc.types
 import primevc.core.traits._

class Ref[V <: ValueObjectWithID](val ref:V#IDType, var vo_! : V = null.asInstanceOf[V])
{
  if (vo_! != null) require(ref != null, "No ref value for Ref[_].vo_! = "+ vo_!);
  
  def empty_? = ref == null
  def vo(implicit proxy:VOProxy[V]) = if (ref == null) None else if (vo_! != null) Some(vo_!)
    else {
      val result = proxy.findByID(ref)
      if (result != None) vo_! = result.get
      result
    }

  override def toString = if (ref != null) "Ref("+ ref +")" else "Ref"
}

class RefArray[V <: ValueObjectWithID](_ref : Array[V#IDType] = null, _voArray : Array[V] = null)
                                      (implicit manifest_IDType : Manifest[V#IDType], voManifest : Manifest[V])
  extends Traversable[V#IDType]
{
  val ref:Array[V#IDType] = if (_ref != null) _ref else new Array[V#IDType](0)
  require(ref != null)

  val voArray:Array[V] = if (_voArray != null) _voArray else new Array[V](ref.length)
  require(voArray != null)
  require(voArray.length == ref.length)

  def empty_? = ref.length == 0
  def length  = ref.length

  def vo(index:Int)(implicit proxy:VOProxy[V]) = if (length == 0) None else voArray(index) match {
    case null => val v = proxy.findByID(ref(index)); v.map(voArray(index) = _); v
    case vo => Some(vo)
  }

  override def toString = if (!empty_?) "RefArray("+ ref.mkString(", ") +")" else "RefArray"

  def foreach[U](f: (V#IDType) => U) = ref.foreach(f)
}

object RefArray
{
  def apply[V <: ValueObjectWithID]()
      (implicit manifest_IDType : Manifest[V#IDType], voManifest : Manifest[V]): RefArray[V] =
    new RefArray[V]

  def apply[V <: ValueObjectWithID](refs : Array[V#IDType])
      (implicit manifest_IDType : Manifest[V#IDType], voManifest : Manifest[V]): RefArray[V] =
    new RefArray(refs, new Array[V](refs.length))

  def apply[V <: ValueObjectWithID](refs : Array[V#IDType], vo:Array[V])
      (implicit manifest_IDType : Manifest[V#IDType], voManifest : Manifest[V]): RefArray[V] =
      new RefArray(refs, vo)

  def apply[V <: ValueObjectWithID](refs : java.util.List[V#IDType])
      (implicit manifest_IDType : Manifest[V#IDType], voManifest : Manifest[V]): RefArray[V] =
  {
    val arr = scala.collection.JavaConversions.asScalaIterable(refs).toArray
    new RefArray(arr, new Array[V](arr.length))
  }
}
