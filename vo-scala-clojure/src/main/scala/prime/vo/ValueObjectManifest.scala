package prime.vo;
 import prime.vo.source._
 import prime.types._
 import clojure.lang.Keyword


trait ValueObjectManifest[VOType <: ValueObject]
{
  val VOType         : Manifest[VOType];
  val ID             : Int;

  val lastFieldIndex : Int;
  def fieldIndexMask : Int;

  /** Find the ValueObjectField for name, or throws NoSuchFieldException. */
  def apply(index : Int)   : ValueObjectField[VOType];
  /** Find the ValueObjectField for name, or throws NoSuchFieldException. */
  def apply(name : String) : ValueObjectField[VOType];

  def firstIndexSet(data : VOType) : Int;
  def  nextIndexSet(data : VOType, index : Int) : Int;

  // ----------------
  // Concrete members
  // ----------------
  @inline final def isFull(data : VOType) = data.voIndexSet == fieldIndexMask;
  
  @inline final def index(n : Int) : Int = {
    val index = if (n <= 32) n else (n & 0xFF);
    if ((fieldIndexMask & (1 << index)) != 0) index
    else throw new java.lang.NoSuchFieldException("n=" + n + ", index=" + index)
  }

  final def index(key : Any) : Int = key match {
    case key:Int     => index(key)
    case key:Long    => index(key.toInt)
    case key:Keyword => apply(key.sym.getName).index
    case key:Symbol  => apply(key.name).index
    case key:String  => apply(key).index
    case _           => apply(key.toString).index
  }

  final def symbol (idx:  Int) = apply(idx).symbol;
  final def keyword(idx:  Int) = apply(idx).keyword;

  final def keyword(key: String) : Keyword = apply(key).keyword
  final def keyword(key: Symbol) : Keyword = apply(key.name).keyword
  final def keyword(key: Any)    : Keyword = key match {
    case key:Keyword => key
    case key:Symbol  => keyword(key.name)
    case key:Int     => keyword(key)
    case _           => keyword(key.toString)
  }
}

abstract class ValueObjectManifest_1[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = manifest[VOType];

  final def fieldIndexMask = 1 << lastFieldIndex;

  final def firstIndexSet(data : VOType)              = if (data.contains(lastFieldIndex)) lastFieldIndex      else -1;
  final def  nextIndexSet(data : VOType, index : Int) = if (index < lastFieldIndex)        firstIndexSet(data) else -1;
}

abstract class ValueObjectManifest_N[VOType <: ValueObject : Manifest] extends ValueObjectManifest[VOType]
{
  val VOType : Manifest[VOType] = implicitly[Manifest[VOType]];

  protected val fields : Array[ValueObjectField[VOType]];
  val fieldIndexMask : Int;

  
  final def apply(idx : Int) = fields(index(idx));
  
  final def nextIndexSet(data : VOType, startIndex : Int) : Int = {
    var k = startIndex;
    do {
      k += 1;
      if (data.contains(k)) return k;
    }
    while (k < this.lastFieldIndex);

    -1; // nothing set after index
  }
  final def firstIndexSet(data : VOType) = nextIndexSet(data, -1)
}


abstract class ValueObjectField[VO <: ValueObject](
  val id           : Int,
  val index        : Int,
  val name         : String,
  val symbol       : Symbol,
  val keyword      : clojure.lang.Keyword,
  val valueType    : ValueType,
  val defaultValue : Any
){
  /* TODO *
    - Implement clojure's KeywordThunk optimization thingy
  */

  def this(id:Int, index:Int, symbol:Symbol, valueType:ValueType, defaultValue:Any) = this(id, index, symbol.name, symbol, Keyword.intern(null, symbol.name), valueType, defaultValue)

  def apply(vo  : VO) : Any;
  def apply(src : ValueSource, orElse : Any) : Any = src.anyAt(name, index, orElse);

  def get(vo : ValueObject) : Any =  vo match { case vo : VO => this(vo); case _ => null; }
  def in (vo : VO)                = (vo.lazyIndexSet & index) != 0;
}

abstract class VOValueObjectField[VO <: ValueObject, T <: ValueObject](
  id           : Int,
  index        : Int,
  symbol       : Symbol,
  voCompanion  : ValueObjectCompanion[T], //[_ <: ValueObject],
  override val defaultValue : T,
  ref          : Boolean
) extends ValueObjectField[VO](id, index, symbol.name, symbol, Keyword.intern(null, symbol.name), ValueTypes.Tdef(voCompanion, ref), defaultValue) {

  override def apply(vo : VO) : T;

  override def in(vo : VO) = apply(vo) != defaultValue;

  def apply(self:VO, src:ValueSource, root:ValueSource, lazyVar:T): T = this(src, None) match {
    case null => defaultValue;
    case v:T => v;
    
    case ValueSource(vo) =>
      if (root != src) /*eager*/ voCompanion(vo);
      else /*lazy*/ lazyVar;
    
    case voCompanion(vo) => vo

    case _ => if (root != self.voSource) /*eager*/ this(self) else /*lazy*/ lazyVar;
  }
}

trait IntValueObjectField {
  def apply(vo : ValueObject, notFound : Int) // overkill ?
}
