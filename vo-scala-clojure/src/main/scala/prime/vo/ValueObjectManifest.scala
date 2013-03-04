package prime.vo;
 import prime.vo.source._
 import prime.types._
 import clojure.lang.Keyword


trait ValueObjectManifest[T <: ValueObject]
{
  type VOType = T;
  import ValueObjectManifest._

  val VOType         : Manifest[VOType];
  val ID             : Int;

  /** Mask of which fields bit indexes are used. */
  val fieldIndexMask : Int;
  /** Mask of which fields are lazily initialized. 0 means all fields are eager. */
  val lazyIndexMask  : Int;
  /** Mask of which fields are directly initialized on construction. 0 means all fields are lazy. */
  val eagerIndexMask : Int;
  /** Number of the last bit index used. */
  val lastFieldIndex : Int;
  /** Which bits in the fieldIndexMask are part of any serializable mixins (trait) */
  val mixinIndexMask : Int;

  /** The number of fields defined in the ValueObject */
  def numFields      : Int;
  /** Find the ValueObjectField with the given key, or return null if none found. */
  def findOrNull(key : Int)    : ValueObjectField[VOType];
  def findOrNull(key : String) : ValueObjectField[VOType];

  /** Returns the index of the ValueObjectField for the given `idx`,   or -1 if not found. */
  def index(idx   : Int) : Int;
  /** Returns the index of the ValueObjectField for the given `name`,  or -1 if not found. */
  def index(name  : String) : Int;
  /** Returns the index of the ValueObjectField for the given `field`, or -1 if not found. */
  def index(field : ValueObjectField[_]) : Int;

  val mixins : Array[ValueObjectMixin];
  /** The fields defined in this ValueObject, not in any super types. */
  val metaMixin : ValueObjectMixin;

  val first  : ValueObjectField[VOType];
  def firstFieldSet(data : VOType)                   : ValueObjectField[VOType];
  def  nextFieldSet(data : VOType, startIndex : Int) : ValueObjectField[VOType];

  // ----------------
  // Concrete members
  // ----------------

  /** Find the ValueObjectField with the given id or field-index, or throw NoSuchFieldException. */
  final def apply(idx  : Int)    : ValueObjectField[VOType] = findOrNull(idx)  match { case null => throw new NoSuchFieldException(this, idx);  case f => f }
  /** Find the ValueObjectField with the given name, or throw NoSuchFieldException. */
  final def apply(name : String) : ValueObjectField[VOType] = findOrNull(name) match { case null => throw new NoSuchFieldException(this, name); case f => f }

  /** The number of lower field-index bits reserved for fields defined in a mixin (trait). */
  @inline
  final def mixinIndexBitsReserved = if (mixinIndexMask == 0) 0 else Integer.numberOfTrailingZeros(Integer.highestOneBit(mixinIndexMask)) + 1;
  /** The number of mixins used in `vo`. */
  final def mixinCount(fields: Int) = {
    var count = 0;
    for (m <- mixins) if ((fields & m.fieldIndexMask) != 0) count += 1;
    count;
  }
  final def mixinManifest(id : Int): ValueObjectMixin = if (id == this.ID) metaMixin else { for (m <- mixins) if (m.manifest.ID == id) return m; null; }

  final def index  (key : Keyword) : Int = index(key.sym.getName)
  final def index  (key : Symbol)  : Int = index(key.name)
  final def index_!(key : Any)     : Int = key match {
    case key:Int     => index(key)
    case key:Long    => index(key.toInt)
    case key:Number  => index(key.intValue)
    case key:Keyword => index(key)
    case key:Symbol  => index(key)
    case key:String  => index(key)
    case _           => index(key.toString)
  }

  def findOrNull(key : Any) : ValueObjectField[VOType] = key match {
    case key:Int     => findOrNull(key)
    case key:String  => findOrNull(key)
    case key:Long    => findOrNull(key.toInt)
    case key:Number  => findOrNull(key.intValue)
    case key:Keyword => findOrNull(key.sym.getName)
    case key:Symbol  => findOrNull(key.name)
    case _           => findOrNull(key.toString)
  }

  final def keyword  (idx : Int)    : Keyword = apply(idx).keyword;
  final def keyword  (key : String) : Keyword = apply(key).keyword
  final def keyword  (key : Symbol) : Keyword = apply(key.name).keyword
  final def keyword_!(key : Any)    : Keyword = key match {
    case key:Keyword => key
    case key:Symbol  => keyword(key.name)
    case key:Int     => keyword(key)
    case _           => keyword(key.toString)
  }

  final def symbol   (idx : Int) = apply(idx).symbol;

  final def fieldMask(mask:Int, field : ValueObjectField[VOType]) = mask | (1 << index(field));

  protected def makeMetaMixin = ValueObjectMixin(if (mixins.isEmpty) 0 else java.lang.Integer.numberOfTrailingZeros(java.lang.Integer.highestOneBit(mixins.last.fieldIndexMask)) + 1, this.fieldIndexMask ^ mixinIndexMask, this);

  /** Array of all (initialized) ValueObjectManifests of classes extending VOType.
      If you expect a manifest to be here, but isn't: make sure the ValueObjectManifest is initialized/constructed.
  */
  var subtypes = Array[ValueObjectManifest[_ >: T]]();
  for (mixin <- this.mixins) {
    mixin.manifest.asInstanceOf[ValueObjectManifest[_ <: T]].subtypes +:= this;
  }
}

case class ValueObjectMixin(numberOfIndexBitsShifted : Int, fieldIndexMask : Int, manifest : ValueObjectManifest[_ <: ValueObject]) {
  import Integer._
  /** Returns the field-set masked and left-shifted to 0-position, so that bit 0 is the mixin's field-set 0. */
  @inline
  final def indexBitsShifted(fieldSet : Int) = (fieldSet & fieldIndexMask) >>> numberOfIndexBitsShifted;
}

trait IDField {
  this : ValueObjectManifest[_ <: ValueObject] =>
  val _id : ValueObjectField[VOType];
}

object ValueObjectManifest {
  class NoSuchFieldException(vo : ValueObjectManifest[_], field : String, extraInfo : String) extends java.lang.IllegalArgumentException(
    "\n  The given name or index `"+ field +"` is not part of "+ vo.VOType.erasure.getName + extraInfo
  ){
    def this(vo : ValueObject, field : String) = this(vo.voManifest, field, "\n  Defined field names are:\n    " +
      vo.voCompanion.fields.filter(_ != null).map(_.name).mkString(", ")  + "\n\n"
    );

    def this(vo : ValueObjectManifest[_], field : String) = this(vo, field, "")
    def this(vo : ValueObjectManifest[_], idx   : Int)    = this(vo, idx.toString)
  }

  def valueObjectTraits(cl : Class[_]) : Array[Class[_]] = cl.getInterfaces
    .filter(cl => cl != classOf[ValueObject] && classOf[ValueObject].isAssignableFrom(cl))
       .map(cl => valueObjectTraits(cl) :+ cl)
       .flatten;

  def getMixinManifests[T <: ValueObject](cl : Class[_]) = valueObjectTraits(cl).map(c => Class.forName(c.getName + "$manifest$").getField("MODULE$").get().asInstanceOf[ValueObjectManifest[_ >: T <: ValueObject]]);
}

abstract class ValueObjectManifest_0[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = manifest[VOType];
  def numFields      = 0;
  val fieldIndexMask = 0;
  val lazyIndexMask  = 0;
  val eagerIndexMask = 0;
  val mixinIndexMask = 0;
  val metaMixin      = ValueObjectMixin(0,0,this);
  val first          = null;
  val lastFieldIndex = -1;

  final def findOrNull(idx : Int)    = null;
  final def findOrNull(name: String) = null;

  final def index(idx   : Int)                 : Int = -1;
  final def index(name  : String)              : Int = -1;
  final def index(field : ValueObjectField[_]) : Int = -1;

  final def firstFieldSet(data : VOType)              = null;
  final def  nextFieldSet(data : VOType, index : Int) = null;
}

abstract class ValueObjectManifest_1[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = manifest[VOType];

  def numFields      = 1;
  val fieldIndexMask = 1 << lastFieldIndex;
  val lazyIndexMask  = if (first isLazy) fieldIndexMask else 0;
  val eagerIndexMask = if (first isLazy) 0 else fieldIndexMask;/*
  val mixins         = ValueObjectManifest.getMixinManifests[VOType](VOType.erasure).map(m => ValueObjectMixin(
    if (first != null && first.VOTypeID == m.ID) fieldIndexMask else 0, m
  ));*/
  val mixinIndexMask = mixins.headOption.map(_.fieldIndexMask).getOrElse(0);
  val metaMixin      = makeMetaMixin;


  final def findOrNull(idx : Int)    = if (index(idx) == lastFieldIndex) first else null;
  final def findOrNull(name: String) = if (first.name == name)           first else null;

  final def index(idx   : Int)                 : Int = if (idx == lastFieldIndex || idx == first.id) lastFieldIndex else -1;
  final def index(name  : String)              : Int = if (name == first.name) lastFieldIndex else -1;
  final def index(field : ValueObjectField[_]) : Int = if (field == first) lastFieldIndex else -1;

  final def firstFieldSet(data : VOType)              = if (first in data)          first               else null;
  final def  nextFieldSet(data : VOType, index : Int) = if (index < lastFieldIndex) firstFieldSet(data) else null;
}

abstract class ValueObjectManifest_N[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = implicitly[Manifest[VOType]];

  protected val fields : Array[_ <: ValueObjectField[VOType]];
  def numFields      = fields.length;
  val first          = fields  .find(_ != null).get;
  val lazyIndexMask  = fields.filter(f => f != null && f.isLazy).foldLeft(0)(fieldMask);
  val fieldIndexMask = fields.filter(f => f != null            ).foldLeft(0)(fieldMask);
  val eagerIndexMask = fieldIndexMask ^ lazyIndexMask;
  val lastFieldIndex = index(fields.last);/*
  val mixins         = ValueObjectManifest.getMixinManifests[VOType](VOType.erasure).map(m => ValueObjectMixin(
    fields.filter(f => f != null && f.VOTypeID == m.ID).foldLeft(0)(fieldMask), m
  ));*/
  val mixinIndexMask = mixins.foldLeft(0) { (mask,mixin) => mask | mixin.fieldIndexMask }
  val metaMixin      = makeMetaMixin;


  final def findOrNull(idx : Int)    : ValueObjectField[VOType] = index(idx) match { case -1 => null; case i => fields(i); }
  final def findOrNull(name: String) : ValueObjectField[VOType] = { for (f <- fields) if (f != null && f.name == name) return f; null }

  final def index(n : Int) : Int = {
    if (n <= 32) {
      if ((fieldIndexMask & (1 << n)) != 0) return n;
    } else {
      val fields = this.fields; var i = 0;
      while (i < fields.length) { val f = fields(i); if (f != null && f.id == n) return i; else i += 1; }
    }
    -1;
  }

  final def index(name : String) : Int = {
    val fields = this.fields; var i = 0;
    while (i < fields.length) { val f = fields(i); if (f != null && (f.name == name)) return i; else i += 1; }
    -1;
  }

  final def index(field : ValueObjectField[_]) : Int = {
    val fields = this.fields; var i = 0;
    while (i < fields.length) if (fields(i) eq field) return i; else i += 1;
    -1;
  }

  final def nextFieldSet(data : VOType, startIndex : Int) : ValueObjectField[VOType] = {
    var k = startIndex;
    do {
      k += 1;
      if ((fieldIndexMask & (1 << k)) != 0) {
        val field = apply(k);
        if (field in data) return field;
      }
    }
    while (k < this.lastFieldIndex);

    null; // nothing set after index
  }

  final def firstFieldSet(data : VOType) = nextFieldSet(data, -1)
}


abstract class ValueObjectField[-VO <: ValueObject] protected(
  val id           : Int,
  val name         : String,
  val symbol       : Symbol,
  val keyword      : clojure.lang.Keyword,
  val valueType    : ValueType,
  val defaultValue : Any
)
 extends clojure.lang.ILookupThunk
{
  require(valueType != null, "Field needs a valueType");


  def this(id:Int, symbol:Symbol, valueType:ValueType, defaultValue:Any) = this(id, symbol.name, symbol, Keyword.intern(null, symbol.name), valueType, defaultValue)

  /** True if  `obj` is a type which can contain this ValueObject field. */
  def isFieldOf(obj : AnyRef) : Boolean;

  /** Get the value for this field from `vo`. */
  def apply(vo  : VO) : Any;
  def apply(src : ValueSource, orElse : Any) : Any = src.anyAt(name, id, orElse);

  def get(vo : ValueObject) : Any =  vo match { case vo : VO => this(vo); case _ => null; }

  final def in (vo : VO) = {
    ((vo.initIndexSet & (1 << vo.voManifest.index(this.asInstanceOf[ValueObjectField[vo.VOType]]))) != 0) || (isLazy && apply(vo) != defaultValue);
  }

  def isLazy = valueType isLazy;

  final def VOTypeID = id >>> 8;

  def get(target:AnyRef) = if (this.isFieldOf(target)) prime.vo.util.ClojureSupport.get(this, target.asInstanceOf[VO]) else this;
}

abstract class VOValueObjectField[-VO <: ValueObject, T <: ValueObject] protected(
  id           : Int,
  symbol       : Symbol,
  override val defaultValue : T // Must always refer to the `empty` instance.
) extends ValueObjectField[VO](id, symbol.name, symbol, Keyword.intern(null, symbol.name), ValueTypes.Tdef(defaultValue, false), defaultValue) {

  val voCompanion : ValueObjectCompanion[T] = defaultValue.voCompanion.asInstanceOf[ValueObjectCompanion[T]];

  override def apply(vo : VO) : T;

  @inline
  final def apply(src : ValueSource, bitIndex:Int, orElse : Any) : Any = src.anyAt(name, (id << 8) | bitIndex, orElse);

  @inline
  def apply(self:VO, src:ValueSource, root:ValueSource, lazyVar:T): T = apply(src, self.voManifest.index(this), None) match {
    case null => defaultValue;
    case v if defaultValue.voManifest.VOType.erasure.isInstance(v) => v.asInstanceOf[T];
    case None => if (root eq self.voSource) /*lazy*/ lazyVar else /*eager*/ apply(self);

    case ValueSource(vo) =>
      if (!(root eq src)) /*eager convert to T*/ voCompanion(vo);
      else /*lazy convert*/ if (self.voSource eq ValueSource.empty) null.asInstanceOf[T] else lazyVar;
  }
}

trait IntValueObjectField {
  def apply(vo : ValueObject, notFound : Int) // overkill ?
}
