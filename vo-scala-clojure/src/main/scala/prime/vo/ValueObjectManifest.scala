package prime.vo;
 import prime.vo.source._
 import prime.types._
 import clojure.lang.Keyword


trait ValueObjectManifest[VOType <: ValueObject]
{
  val VOType         : Manifest[VOType];
  val ID             : Int;

  /** Mask of which fields are lazily initialized. 0 means all fields are eager. */
  val lazyIndexMask  : Int;
  /** Mask of which fields are directly initialized on construction. 0 means all fields are lazy. */
  val eagerIndexMask : Int;
  /** Mask of which fields bit indexes are used. */
  val fieldIndexMask : Int;
  /** Number of the last bit index used. */
  val lastFieldIndex : Int;

  /** Find the ValueObjectField with the given id or field-index, or throw NoSuchFieldException. */
  def apply(idx  : Int)    : ValueObjectField[VOType];
  /** Find the ValueObjectField with the given name, or throw NoSuchFieldException. */
  def apply(name : String) : ValueObjectField[VOType];

  def index(idx   : Int) : Int;
  def index(name  : String) : Int;
  def index(field : ValueObjectField[VOType]) : Int;

  val first : ValueObjectField[VOType];
  def firstFieldSet(data : VOType)                   : ValueObjectField[VOType];
  def  nextFieldSet(data : VOType, startIndex : Int) : ValueObjectField[VOType];

  // ----------------
  // Concrete members
  // ----------------

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
}

object ValueObjectManifest {
  object NoSuchFieldException extends java.lang.IllegalArgumentException("The given name or index is not part of this ValueObject.");
}

abstract class ValueObjectManifest_1[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = manifest[VOType];

  val fieldIndexMask = 1 << lastFieldIndex;
  val lazyIndexMask  = if (first isLazy) fieldIndexMask else 0;
  val eagerIndexMask = if (first isLazy) 0 else fieldIndexMask;

  final def apply(idx : Int)    = if (index(idx) == lastFieldIndex) first else throw ValueObjectManifest.NoSuchFieldException;
  final def apply(name: String) = if (first.name == name)           first else throw ValueObjectManifest.NoSuchFieldException;

  final def index(idx   : Int)                      : Int = if (idx == 0 || idx == first.id) 0 else -1;
  final def index(name  : String)                   : Int = if (name == first.name) 0 else -1;
  final def index(field : ValueObjectField[VOType]) : Int = if (field == first) 0 else -1;

  final def firstFieldSet(data : VOType)              = if (first in data)          first               else null;
  final def  nextFieldSet(data : VOType, index : Int) = if (index < lastFieldIndex) firstFieldSet(data) else null;
}

abstract class ValueObjectManifest_N[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = implicitly[Manifest[VOType]];

  protected val fields : Array[_ <: ValueObjectField[VOType]];
  val first          = fields(0);
  val lazyIndexMask  = fields.foldLeft(0) { (mask,field) => mask | (if (field isLazy) 1 << index(field) else 0) }
  val fieldIndexMask = fields.foldLeft(0) { (mask,field) => mask | (1 << index(field))                          }
  val eagerIndexMask = fieldIndexMask ^ lazyIndexMask;
  val lastFieldIndex = index(fields.last);

  final def apply(idx : Int)    : ValueObjectField[VOType] = fields(index(idx));
  final def apply(name: String) : ValueObjectField[VOType] = { for (f <- fields) if (f != null && f.name == name) return f; throw ValueObjectManifest.NoSuchFieldException; }

  final def index(n : Int) : Int = {
    if (n <= 32) {
      if ((fieldIndexMask & (1 << n)) != 0) return n;
    } else {
      var i = 0;
      for (f <- fields) if (f != null && f.id == n) return i; else i += 1;
    }
    throw ValueObjectManifest.NoSuchFieldException;
  }

  final def index(name : String) : Int = {
    var i = 0;
    for (f <- fields) if (f != null && (f.name == name)) return i; else i += 1;
    -1;
  }

  final def index(field : ValueObjectField[VOType]) : Int = {
    var i = 0;
    for (f <- fields) if (f eq field) return i; else i += 1;
    -1;
  }

  final def nextFieldSet(data : VOType, startIndex : Int) : ValueObjectField[VOType] = {
    var k = startIndex;
    do {
      k += 1;
      try {
        val field = apply(k);
        if (field in data) return field;
      }
      catch {
        case ValueObjectManifest.NoSuchFieldException =>
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
){
  require(valueType != null, "Field needs a valueType");

  /* TODO *
    - Implement clojure's KeywordThunk optimization thingy
  */

  def this(id:Int, symbol:Symbol, valueType:ValueType, defaultValue:Any) = this(id, symbol.name, symbol, Keyword.intern(null, symbol.name), valueType, defaultValue)

  def apply(vo  : VO) : Any;
  def apply(src : ValueSource, orElse : Any) : Any = src.anyAt(name, id, orElse);

  def get(vo : ValueObject) : Any =  vo match { case vo : VO => this(vo); case _ => null; }
  final def in (vo : VO) = {
    ((vo.initIndexSet & (1 << vo.voManifest.index(this.asInstanceOf[ValueObjectField[vo.VOType]]))) != 0) || (isLazy && apply(vo) != defaultValue);
  }

  def isLazy = valueType isLazy;
}

abstract class VOValueObjectField[-VO <: ValueObject, T <: ValueObject] protected(
  id           : Int,
  symbol       : Symbol,
  override val defaultValue : T // Must always refer to the `empty` instance.
) extends ValueObjectField[VO](id, symbol.name, symbol, Keyword.intern(null, symbol.name), ValueTypes.Tdef(defaultValue, false), defaultValue) {

  val voCompanion : ValueObjectCompanion[T] = defaultValue.voCompanion.asInstanceOf[ValueObjectCompanion[T]];

  override def apply(vo : VO) : T;

  def apply(self:VO, src:ValueSource, root:ValueSource, lazyVar:T): T = apply(src, None) match {
    case None => if (root != self.voSource) /*eager*/ apply(self) else /*lazy*/ lazyVar;
    case null => defaultValue;
    case v: T => v;

    case ValueSource(vo) =>
      if (root != src) /*eager convert to T*/ voCompanion(vo);
      else /*lazy convert*/ if (self.voSource == ValueSource.empty) null.asInstanceOf[T] else lazyVar;
  }
}

trait IntValueObjectField {
  def apply(vo : ValueObject, notFound : Int) // overkill ?
}
