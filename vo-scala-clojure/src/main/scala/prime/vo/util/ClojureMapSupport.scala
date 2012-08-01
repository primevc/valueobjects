package prime.vo.util
 import prime.vo._
 import prime.vo.source._
 import clojure.lang.{Var, RT, Util, Keyword, MapEntry, MapEquivalence}
 import clojure.lang.{ASeq, ISeq, Counted, IPersistentCollection, IPersistentMap, IPersistentVector, IPending, Indexed}

trait ClojureMapSupport extends IPersistentMap
 with MapEquivalence
 with ClojureFn
 with Indexed
 with IPending
 with JavaMapSupport[clojure.lang.Keyword] {
  this: ValueObject =>

  protected var _hash : Int = -1;
  override def hashCode = if(_hash != -1) _hash else {
    _hash = clojure.lang.APersistentMap.mapHash(this);
    _hash
  }

  // ---
  // Associative
  // ---
  final def assoc       (key: Any, value: Any) : IPersistentMap = {
    val k = voManifest.index_!(key);
    if (k >= 0)
      assoc(k, value)
    else {
      // http://grepcode.com/file/repo1.maven.org/maven2/org.clojure/clojure/1.3.0/clojure/lang/PersistentArrayMap.java#37
      // Create map from VO + new value
      //PersistentArrayMap.create()
      null
    }
  }
  /** assoc exclusive: Only assoc when 'key is not present. */
  final def assocEx     (key: Any, value: Any) = {
    val k = voManifest.index_!(key);
    require(!containsKey(k), "Index "+k+" (key="+key+") invalid or already present")
    assoc(k, value)
  }
  
  final def entryAt(key: Any) = {
    val k = voManifest.index_!(key);
    if (k >= 0) new MapEntry(voManifest.keyword(k), nth(k))
    else null
  }

  // IPersistentCollection
  def empty : this.type = voCompanion.empty.asInstanceOf[this.type];

  /** Used by clojure.core/conj */
  def cons (o: Any) : IPersistentCollection = o match
  {
    case e : java.util.Map.Entry[_,_] => this.assoc(e.getKey(), e.getValue());
    
    case v : IPersistentVector =>
      if(v.count != 2)
        throw new IllegalArgumentException("Vector arg to map conj must be a pair");
      assoc(v.nth(0), v.nth(1));

    //TODO: Benchmark if typecheck before unapply matters
    case v : ValueSource => conj(v);
    case ValueSource(v)  => conj(v);

    case _ =>
       //FIXME: pretty inefficient!
      var ret : IPersistentMap = this;
      var es = RT.seq(o);
      while (es != null)
      {
        val e = es.first.asInstanceOf[java.util.Map.Entry[_,_]];
        ret = ret.assoc(e.getKey(), e.getValue());

        es = es.next();
      }
      ret;
  }

  protected def hasSameKeysAndValues(m : java.util.Map[_,_]) : Boolean = {
    foreach { (field, value) =>
      val foundKey = if (m.containsKey(field.keyword))           field.keyword
                else if (m.containsKey(field.name))              field.name
                else if (m.containsKey(field.id))                field.id
                else {
                  val idx : Any = voManifest.index(field);
                  if (m.containsKey(idx)) idx;
                  else null;
                }
      
      if(foundKey == null || !Util.equiv(value, m.get(foundKey)))
        return false;
    }
   
    true;
  }

  override def equals(obj : Any) : Boolean =
    if (obj.asInstanceOf[AnyRef] eq this) true
    else obj match
    {
      case vo : VOType =>
        println("Comparing equal typed VOs")
        // Uses lazyIndexSet to prevent unneccesary realizing of sub VOs.
        if ((this.lazyIndexSet & vo.lazyIndexSet) != vo.lazyIndexSet)
          false;
        else {
          println(" - Comparing all properties")
          foreach { (field, value) => if (!Util.equiv(value, field(vo))) return false; }
          true;
        }

      case m : java.util.Map[_,_] if m.size == this.count =>
        m match {
          case vo : ValueObject =>
            println("Comparing different types VOs: "+this.getClass+ " and "+ m.getClass)
            foreach { (field, value) =>
              // assert(field != null, "field");
              // assert(field.name != null, "fieldname");
              // assert(vo(field.name) != null, "vo(field.name)");
              if (!Util.equiv(value, field.get(vo))) return false;
              //TODO: Bench
              //vo(field.index) != value) return false;
            }
            true;

          case _ =>
            println("Comparing VO to Map")
            hasSameKeysAndValues(m)
        }

      case _ => false
    }

  def equiv(obj: AnyRef) : Boolean = if (obj eq this) true else obj match {
    case m : java.util.Map[_,_] if (!(m.isInstanceOf[IPersistentMap] && !m.isInstanceOf[MapEquivalence])) =>
      println("Comparing VO to MapEquivalence")
      this.equals(obj)

    case _ => false
  }
  
  // Seqable
  final class Seq(field : ValueObjectField[VOType], meta: IPersistentMap = null) extends ASeq(meta) with Counted
  {
    def first = new MapEntry(field.keyword, field(self));
    def next  = {
      val nextField = voManifest.nextFieldSet(self, voManifest.index(field));
      if (nextField != null) new Seq(nextField) else null;
    }
    
    override def count = java.lang.Integer.bitCount( voIndexSet & (0x7FFFFFFF << (voManifest.index(field) + 1)) );
    
    def withMeta(meta : IPersistentMap) = new Seq(field, meta);
  }
  def seq = {
    val f = voManifest.firstFieldSet(self);
    if (f != null) new Seq(f) else null;
  }

  // ---
  // IPersistentMap
  // ---
  def without     (key: Any): this.type = without(voManifest.index_!(key));
  
  // IPersistentMap interfaces
  // ILookup
  final def valAt (key: Any, notFound: AnyRef): AnyRef = nth(voManifest.index_!(key), notFound).asInstanceOf[AnyRef]
  final def valAt (key: Any): AnyRef = valAt(key, null)

  // Iterable
  /**
    Always returns null. Why? I'll tell you why:

    user=> (.iterator {:wut :wat})
    ClassCastException clojure.lang.PersistentArrayMap cannot be cast to clojure.lang.PersistentHashMap  user/eval1540 (NO_SOURCE_FILE:1)
   */
  override def iterator = null;// : java.util.Iterator[AnyRef] = null;

  // ---
  // Map
  // ---
  final def entrySet =
  {
    import java.util._

    new AbstractSet[Map.Entry[Keyword,Any]]() {
      def size       = ClojureMapSupport.this.count;
      def iterator() = ClojureMapSupport.this.iterator().asInstanceOf[java.util.Iterator[java.util.Map.Entry[clojure.lang.Keyword,Any]]]
      
      override def hashCode = ClojureMapSupport.this.hashCode;
      
      override def contains(obj : Any) = obj match {
        case e : Map.Entry[_,_] =>
          val index = voManifest.index_!(e.getKey);
          index != -1 && Util.equals(nth(index), e.getValue())

        case _ => false;
      }
    }
  }

  final def keySet = new java.util.AbstractSet[Keyword]() {
    def size       = ClojureMapSupport.this.count;
    def iterator() = new VOIterator[Keyword]() {
      def next = {
        val v = field.keyword;
        nextField();
        v
      }
    }

    def contains(obj : Keyword) = ClojureMapSupport.this.containsKey(obj);
  }


  // ---
  // IFn: VO as function from key to value
  // ---
  override final def invoke  (key: AnyRef) : AnyRef = nth(voManifest.index_!(key))
  override final def invoke  (key: AnyRef, notFound: AnyRef) : AnyRef = nth(voManifest.index_!(key), notFound.asInstanceOf[AnyRef])

  final def applyTo (arglist: ISeq) = RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case 2 => invoke(arglist.first, arglist.next.first);
    case n => throwArity(n);
  }

  // ---
  // Indexed
  // ---
  final def nth(index : Int) = voManifest(index).get(self).asInstanceOf[AnyRef];
  final def nth(index : Int, notFound:AnyRef) = if (this.contains(index)) this.nth(index) else notFound;
}
