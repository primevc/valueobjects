package prime.vo.util
 import prime.vo._

trait JavaMapSupport[KeyType] extends java.util.Map[KeyType,Any]
 with java.io.Serializable {
  this: ValueObject =>
  
  import java.util._

  protected def keySet(field : ValueObjectField[_]) : KeyType;

  // ---
  // Map
  // ---
  final def containsKey (key: Any) = this.contains(voManifest.index_!(key));

  /** Not final: Search could be short circuited in subclasses. Subclass knows what types can possibly be stored. */
  def containsValue(value : Any) : Boolean = { foreach { (key, value2) => if (value == value2) return true; }; false; }

  final def get(key: Any)     = try voManifest(voManifest.index_!(key))(self) catch { case e:NoSuchFieldException => null };
  final def isEmpty           = initIndexSet == 0 && voIndexSet == 0;
  final def size              = JavaMapSupport.this.count;

  abstract class VOIterator[T] extends java.util.Iterator[T]()
  {
    var field     = voManifest.firstFieldSet(self);
    def hasNext   = field != null;
    def remove    = throw new UnsupportedOperationException();

    def nextField() {
      field = voManifest.nextFieldSet(self, voManifest.index(field))
    }
  }

  final def values = new java.util.AbstractCollection[Any]() {
    def size = JavaMapSupport.this.count;

    def iterator = new VOIterator[Any] {
      def next = {
        val v = field(self);
        nextField();
        v
      }
    }
  }

  final def keySet = new java.util.AbstractSet[KeyType]() {
    def size       = JavaMapSupport.this.count;
    def iterator() = new VOIterator[KeyType]() {
      def next = {
        val v = keySet(field);
        nextField();
        v
      }
    }

    def contains(obj : KeyType) = JavaMapSupport.this.containsKey(obj);
  }

  final def entrySet =
  {
    import java.util._

    new AbstractSet[Map.Entry[KeyType,Any]]() {
      def size       = JavaMapSupport.this.count;
      def iterator() = new VOIterator[Map.Entry[KeyType,Any]]() {
        def next = {
          val k = keySet(field);
          val v = field(self);
          nextField();
          new AbstractMap.SimpleImmutableEntry(k,v)
        }
      }

      override def hashCode = JavaMapSupport.this.hashCode;

      override def contains(obj : Any) = obj match {
        case e : Map.Entry[_,_] =>
          val index = voManifest.index_!(e.getKey);
          index != -1 && voManifest(index)(self) == e.getValue()

        case _ => false;
      }
    }
  }


  // Unsupported operations
  final def put(key:KeyType, value:Any)             = throw new UnsupportedOperationException();
  final def putAll(t:java.util.Map[_ <: KeyType,_]) = throw new UnsupportedOperationException();
  final def remove(key:Any)                         = throw new UnsupportedOperationException();
  final def clear()                                 = throw new UnsupportedOperationException();
}
