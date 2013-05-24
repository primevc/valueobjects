package prime.vo.util
 import prime.vo._
 import prime.vo.source._
 import clojure.lang.{Var, RT, Util, IFn, IKeywordLookup, ILookupThunk, Keyword, MapEntry, MapEquivalence}
 import clojure.lang.{ASeq, ISeq, Counted, IPersistentCollection, IPersistentMap, IPersistentVector, IPending, Indexed}

object ClojureSupport {
  /** Wraps the obj if it is a Scala Seq type */
  def clojurify[T <: ValueObject](field : ValueObjectField[T], value : Any): AnyRef = value match {
    case v:scala.collection.Seq[_] => ClojureVectorSupport.asClojure(v.asInstanceOf[scala.collection.Seq[Any]])(field.valueType.convert,null);
    case v:AnyRef => v;
  }

  /** Returns the value, or wrapped Scala collection value when given field is set, or notFound if field is not set. */
  final def valAt[T <: ValueObject](f : ValueObjectField[T], obj : T, notFound: AnyRef): AnyRef = {
    if (f in obj) clojurify(f, f(obj));
    else notFound;
  }

  /** Returns
    - the value (or wrapped Scala collection) when given field is set,
    - or empty value-object instance if field is not set and is a ValueObject field
    - or null if field is not set, and it is a simple value field.
  */
  final def valAt[T <: ValueObject](f : ValueObjectField[T], obj : T) : AnyRef = f match {
    case f : VOValueObjectField[_,_] => f(obj);
    case f : ValueObjectField[_]     => {
      if      (f in obj)           clojurify(f, f(obj))
      else if (f.valueType.isLazy) clojurify(f, f.defaultValue)
      else                         null;
    }
    case null => null;
  }


  RT.loadResourceScript("prime/vo.clj");
  val seqFieldFn = RT.`var`("prime.vo", "*voseq-key-fn*");
}

trait ClojureMapSupport extends IPersistentMap
 with MapEquivalence
 with ClojureFn
 with Indexed
 with IPending
 with IKeywordLookup
 with JavaMapSupport[Keyword] {
  this: ValueObject =>

  import ClojureSupport.clojurify;

  private[this] var _hash : Int = -1;
  override def hashCode = if(_hash != -1) _hash else {
    _hash = this.getClass.hashCode ^ clojure.lang.APersistentMap.mapHash(this);
    _hash
  }

  // ---
  // Associative
  // ---
  final def assoc       (key: Any, value: Any) : IPersistentMap = {
    val f = voManifest.findOrNull(key);
    if (f != null)
      assoc(f, value)
    else {
      // http://grepcode.com/file/repo1.maven.org/maven2/org.clojure/clojure/1.3.0/clojure/lang/PersistentArrayMap.java#37
      // Create map from VO + new value
      //PersistentArrayMap.create()
      this
    }
  }
  /** assoc exclusive: Only assoc when 'key is not present. */
  final def assocEx     (key: Any, value: Any) = {
    val k = voManifest.index_!(key);
    require(!containsKey(k), "Index "+k+" (key="+key+") invalid or already present")
    assoc(k, value)
  }
  
  final def entryAt(key: Any) = try {
    val f = voManifest.findOrNull(key);
    if (f != null && (f in self))
      new MapEntry(f.keyword, clojurify(f, f(self)));
    else
      null;
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
      
      if(foundKey == null || value != m.get(foundKey))
        return false;
    }
   
    true;
  }

  override def equals(obj : Any) : Boolean =
    if (obj.asInstanceOf[AnyRef] eq this) true
    else obj match
    {
      case vo : ValueObject =>
        // Uses initIndexSet to prevent unneccesary realizing of sub VOs.
        if (vo.getClass == this.getClass && (this.initIndexSet & voManifest.eagerIndexMask) != (vo.initIndexSet & voManifest.eagerIndexMask)) {
          false;
        }
        else
        {
          val selfT = this.voManifest.VOType.erasure;
          val   voT = vo  .voManifest.VOType.erasure;
          if      (selfT.isAssignableFrom(voT)) {
            this.foreach { (field, value) => if (value != field(vo.asInstanceOf[VOType])     ) return false; }
          }
          else if (voT.isAssignableFrom(selfT))
            vo  .foreach { (field, value) => if (value != field(this.asInstanceOf[vo.VOType])) return false; }
          else
            return false;

          vo.count == this.count;
        }

      case m : java.util.Map[_,_] =>
        hasSameKeysAndValues(m) && m.size == this.count;

      case _ => false
    }

  def equiv(obj: AnyRef) : Boolean = if (obj eq this) true else obj match {
    case m : java.util.Map[_,_] if (!(m.isInstanceOf[IPersistentMap] && !m.isInstanceOf[MapEquivalence])) =>
      this == obj;

    case _ => false
  }

  // Seqable
  final class Seq(seqFieldFn : IFn, remainingBits : Int, index : Int, meta: IPersistentMap = null) extends ASeq(meta) with Counted
  {
    import java.lang.Integer._

    def field = voManifest(index);
    def first = {
      val f = field;
      new MapEntry(if (seqFieldFn == null) f.keyword else seqFieldFn.invoke(f), clojurify(f, f(self)));
    }

    def next  = if (remainingBits != 0) {
      val nextOffset = 1 + numberOfTrailingZeros(remainingBits);
      val newBits = remainingBits >>> nextOffset;
      assert(remainingBits != newBits, remainingBits + " should != " + newBits);
      assert(index != (index + nextOffset), nextOffset + " should != " + index +" + "+ nextOffset);
      new Seq(seqFieldFn, newBits, index + nextOffset);

    } else null;

    override def count = 1 + bitCount(remainingBits);

    def withMeta(meta : IPersistentMap) = new Seq(seqFieldFn, remainingBits, index, meta);
  }

  def seq(bits : Int) = {
    if (bits != 0) {
      val index = java.lang.Integer.numberOfTrailingZeros(bits);
      new Seq(ClojureSupport.seqFieldFn.deref.asInstanceOf[IFn], bits >>> (1 + index), index)
    }
    else null;
  }
  def seq = this.seq(voIndexSet);

  // ---
  // IPersistentMap
  // ---
  def without     (key: Any): this.type = without(voManifest.index_!(key));
  
  // IPersistentMap interfaces
  // ILookup
  final def valAt (key: Any) : AnyRef = voManifest.findOrNull(key) match {
    case null => null;
    case f    => ClojureSupport.valAt(f, self);
  }
  final def valAt (key: Any, notFound: AnyRef): AnyRef = voManifest.findOrNull(key) match {
    case null => null;
    case f    => ClojureSupport.valAt(f, self, notFound);
  }

  // IKeywordLookup
  final def getLookupThunk(key: Keyword): ILookupThunk = voManifest.findOrNull(key) match {
    case null => throw new ValueObjectManifest.NoSuchFieldException(this, key.toString);
    case f    => f.asInstanceOf[ILookupThunk];
  }

  // Iterable
  /**
    Always returns null. Why? I'll tell you why:

    user=> (.iterator {:wut :wat})
    ClassCastException clojure.lang.PersistentArrayMap cannot be cast to clojure.lang.PersistentHashMap  user/eval1540 (NO_SOURCE_FILE:1)
   */
  override def iterator = null;// : java.util.Iterator[AnyRef] = null;
  final protected def   keySet(field : ValueObjectField[_]) = field.keyword;
  final protected def entrySet(field : ValueObjectField[_]) = new MapEntry(field.keyword, field.get(ClojureMapSupport.this)).asInstanceOf[java.util.Map.Entry[Keyword,Any]];

  // ---
  // IFn: VO as function from key to value
  // ---
  override final def invoke  (key: AnyRef)                   : AnyRef = voManifest.findOrNull(key) match {
    case null => throw new ValueObjectManifest.NoSuchFieldException(this, key.toString);
    case f    => ClojureSupport.valAt(f, self)
  }
  override final def invoke  (key: AnyRef, notFound: AnyRef) : AnyRef = voManifest.findOrNull(key) match {
    case null => throw new ValueObjectManifest.NoSuchFieldException(this, key.toString);
    case f    => ClojureSupport.valAt(f, self, notFound)
  }

  final def applyTo (arglist: ISeq) = RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case 2 => invoke(arglist.first, arglist.next.first);
    case n => throwArity(n);
  }

  // ---
  // Indexed
  // ---
  final def nth(index : Int)                  = ClojureSupport.valAt(voManifest(index), self);
  final def nth(index : Int, notFound:AnyRef) = ClojureSupport.valAt(voManifest(index), self, notFound);
}
