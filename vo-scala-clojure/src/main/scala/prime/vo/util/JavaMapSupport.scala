package prime.vo.util
 import prime.vo._

trait JavaMapSupport[KeyType] extends java.util.Map[KeyType,Any]
 with java.io.Serializable {
  this: ValueObject =>
  
  import java.util._

  // ---
  // Map
  // ---
  final def containsKey (key: Any) = this.contains(voManifest.index(key));

  /** Not final: Search could be short circuited in subclasses. Subclass knows what types can possibly be stored. */
  def containsValue(value : Any) : Boolean = { foreach { (key, value2) => if (value == value2) return true; }; false; }

  final def get(key: Any)     = try voManifest(voManifest.index(key))(self) catch { case e:NoSuchFieldException => null };
  final def isEmpty           = voIndexSet == 0;
  final def size              = JavaMapSupport.this.count;

  abstract class VOIterator[T] extends java.util.Iterator[T]()
  {
    def remove  = throw new UnsupportedOperationException();
    
    def nextIndex() { 
      index = JavaMapSupport.this.voManifest/*  Scala's type system is too limited */.asInstanceOf[ValueObjectManifest[JavaMapSupport.this.type]]
        .nextIndexSet(JavaMapSupport.this, index)
    }
    
    var index     = -1; nextIndex();
    def hasNext   = index != -1;
  }

  final def values = new java.util.AbstractCollection[Any]() {
    def size = JavaMapSupport.this.size;

    def iterator = new VOIterator[Any] {
      def next = {
        val v = voManifest(index)(self);
        nextIndex();
        v
      }
    }
  }

  // Unsupported operations
  final def put(key:KeyType, value:Any)             = throw new UnsupportedOperationException();
  final def putAll(t:java.util.Map[_ <: KeyType,_]) = throw new UnsupportedOperationException();
  final def remove(key:Any)                         = throw new UnsupportedOperationException();
  final def clear()                                 = throw new UnsupportedOperationException();
}
