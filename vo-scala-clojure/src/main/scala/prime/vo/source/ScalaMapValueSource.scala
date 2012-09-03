package prime.vo.source;
 import prime.vo.source._

class ScalaMapValueSource_String(map : scala.collection.Map[String,_]) extends ValueSource with NoPrimitives {
  def contains (name: String, ignored: Int)                : Boolean = map contains name;
  def anyAt    (name: String, ignored: Int, notFound: Any) : Any     = map get name getOrElse notFound;
}

class ScalaMapValueSource_Symbol(map : scala.collection.Map[Symbol,_]) extends ValueSource with NoPrimitives {
  def contains (name: String, ignored: Int)                : Boolean = map contains Symbol(name);
  def anyAt    (name: String, ignored: Int, notFound: Any) : Any     = map get Symbol(name) getOrElse notFound;
}

class ScalaMapValueSource_Int(map : scala.collection.Map[Int,_]) extends ValueSource with NoPrimitives {
  def contains (ignored: String, idx : Int)                : Boolean = map.contains(idx) || map.contains(idx & 0xFF) || map.contains(idx >>> 8);
  def anyAt    (ignored: String, idx : Int, notFound: Any) : Any     = map.get(idx) orElse map.get(idx & 0xFF) orElse map.get(idx  >>> 8) getOrElse notFound;
}

class ScalaMapValueSource[K, V](map : scala.collection.Map[K,V]) extends ValueSource with NoPrimitives {
  def find[T : Manifest](key : T) : Option[V] = key match {
    case key : K => map.get(key)
    case _       => None
  }

  def findKey  (name: String, orIdx: Int)                : Option[V] = find(name) orElse find(Symbol(name)) orElse find(orIdx) orElse find(orIdx & 0xFF) orElse find(orIdx >>> 8);
  def contains (name: String, orIdx: Int)                : Boolean   = findKey(name, orIdx) isDefined;
  def anyAt    (name: String, orIdx: Int, notFound: Any) : Any       = findKey(name, orIdx) getOrElse(notFound);
}

object Map {
  def apply[K,V](keytype:Class[_], map : scala.collection.Map[K,V]) : ValueSource =
         if (keytype == classOf[String]) new ScalaMapValueSource_String(map.asInstanceOf[Map[String,_]]);
    else if (keytype == classOf[Symbol]) new ScalaMapValueSource_Symbol(map.asInstanceOf[Map[Symbol,_]]);
    else if (keytype == classOf[Int])    new ScalaMapValueSource_Int   (map.asInstanceOf[Map[Int,   _]]);
    else                                 new ScalaMapValueSource[K,V]  (map);

  def apply(map : Map[_,_]) : ValueSource = {
    val keyTypes = map.keySet.map(_.getClass);
    if (keyTypes.size == 1) apply(keyTypes head, map)
    else new ScalaMapValueSource(map);
  }

  implicit def mapAsValueSource(map : Map[String,_]) = new ScalaMapValueSource_String(map);
  implicit def mapAsValueSource(map : Map[Symbol,_]) = new ScalaMapValueSource_Symbol(map);
  implicit def mapAsValueSource(map : Map[Int,   _]) = new ScalaMapValueSource_Int   (map);
  implicit def mapAsValueSource(map : Map[_,     _]) = apply(map);

  prime.vo.source.ValueSource;

  import clojure.lang.RT.{ `var` => v }
  v("clojure.core", "eval").invoke(v("clojure.core","read-string").invoke("""
    (extend-type scala.collection.Map
      prime.vo.source/ValueSourceable
    (as-source
      ([map]      (prime.vo.source.Map/apply map))
      ([map kind] (prime.vo.source.Map/apply map))))"""));
}
