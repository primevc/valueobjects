package prime.vo
 import prime.vo.source._
 import prime.vo.util._

trait ValueObject extends ValueSource
// with IndexedValueSource[Any]
// with NamedValueSource[Any]
{
  type VOType <: ValueObject;

  /** Hints which fields are likely different from the `voSource`.
      Could return a false positive, but _not_ a false negative. */
  protected[prime] def srcDiff  : Int;

  /** Returns either:
      - A new copy with data from `src` and with `new.voSource == root`.
      - `this`:  if `src` contains no different data.
      - `empty`: if `src` contains no data related to this.
  */
  protected[prime] def conj(src : ValueSource, root : ValueSource) : this.type;

  //
  // ValueObject members
  //

  /**
    Acts like a bit-set. Every bit indicates there is a value for that index.
    Example: if (voIndexSet & (1 << 20) != 0) { ...this(20) has a value... }

    Realizes this ValueObject.
  */
  def voIndexSet  : Int;
  /**
    Directly read the `voIndexSet` bits that were set at construction and do not require realizing fields from ValueSource (such as Vectors and sub VOs).

    Does not realize this ValueObject.
  */
  def initIndexSet: Int;

  def voManifest  : ValueObjectManifest [VOType];
  def voCompanion : ValueObjectCompanion[VOType];
  /** The data-source this ValueObject wraps. */
  val voSource    : ValueSource;

  /** Iterates over all set-fields and their values. */
  def foreach (b : (ValueObjectField[VOType],Any) => Unit) : Unit;
  /** True if given property ID, or field index has a value. */
  def contains(idx : Int)    : Boolean;
  /** True if given property name has a value. */
  def contains(key : String) : Boolean;
  /** Number of fields' with a non-default value. */
  def count : Int;

  //
  // Default implementations
  //
  @inline final def self = this.asInstanceOf[VOType];

  @inline final def contains(key : Symbol) : Boolean = this.contains(key.name);

  /** True if all Vectors and sub-ValueObjects recursively have been initialized from their ValueSources. */
  def isRealized : Boolean;
  /** Returns itself with all fields realized. */
  def realized   : VOType;

  /**
    Conjoin `src` and `this`.

    Will return a new copy with all fields from `src` and any other fields from `this`.
    If `src` has no different values or is empty, `this` is returned.
  */
  final def    conj(src : ValueSource)         : this.type = conj(src, this.voSource);

  def         assoc(field : ValueObjectField[VOType], value : Any) : this.type = conj(new SingleValueSource(field.name, value));

  def         assoc(idx : Int,    value : Any) : this.type = assoc(voManifest(idx), value);
  def         assoc(key : String, value : Any) : this.type = assoc(voManifest(key), value);
  final def   assoc(key : Symbol, value : Any) : this.type = assoc(key.name, value);
  final def without(idx : Int)                 : this.type = assoc(idx, null);
  final def without(key : String)              : this.type = assoc(key, null);
  final def without(key : Symbol)              : this.type = without(key.name);
}

//
// Additional abstractions
//

object TraversableValueObject {
  def apply[V <: ValueObject](vo : V) : scala.collection.immutable.Traversable[(ValueObjectField[V#VOType],Any)] =
    new scala.collection.immutable.Traversable[(ValueObjectField[V#VOType],Any)] {
      final def foreach[U](fn: ((ValueObjectField[V#VOType], Any)) ⇒ U): Unit = vo.foreach((f,v) => fn((f.asInstanceOf[ValueObjectField[V#VOType]], v)));
    }
}

trait ID {
  type IDType;
  def _id : IDType;
}

trait LeafNode {
  this : ValueObjectBase =>

  @inline override def contains(id : Int) : Boolean = (initIndexSet & (1 << voManifest.index(id))) != 0;

  final def isRealized = true;
  final def realized   = self;
}

trait BranchNode {
  this : ValueObjectBase =>

  @inline override def contains(id : Int) : Boolean = {
    val idx = voManifest.index(id);
    val msk = 1 << idx;
    if ((voManifest.lazyIndexMask & msk) != 0) (initIndexSet & msk) != 0; else voManifest(idx) in self;
  }
}


// ---------------
// VO Base classes
// ---------------

abstract class ValueObjectBase extends ValueObject with ClojureMapSupport {
  def contains(name: String)             : Boolean = voManifest.findOrNull(name) match { case null => false; case f => f in self }
  def contains(name: String, orIdx: Int) : Boolean = contains(name) || contains(voManifest.index(orIdx));

//  final def apply(idx:  Int,    notFound: Any) : Any = apply(null, idx, notFound)
//  final def apply(name: String, notFound: Any) : Any = apply(name,  -1, notFound)
  def anyAt(name: String, idx: Int, notFound: Any) : Any = {
    val field   =  voManifest.findOrNull(name) match {
      case null => voManifest.findOrNull(idx);
      case f    => f;
    }
    if (field != null && (field in self)) field(self);
    else notFound;
  }

  def sourceAt(name: String, idx: Int, notFound: ValueSource) = ValueSource(anyAt(name,idx,notFound))

  def count = java.lang.Integer.bitCount(this.voIndexSet);

  override def toString = voManifest.VOType.erasure.getSimpleName + "(" +
    TraversableValueObject(this).map({ case (f, v) => f.symbol.name + " = " + v }).mkString(", ") +
  ")";
}

abstract class ValueObject_0 extends ValueObjectBase {
  override def count     = 0;
  def initIndexSet : Int = 0;
  def voIndexSet   : Int = 0;
  def srcDiff      : Int = 0;

  @inline final override def contains(none: Int)               = false;
  @inline final override def contains(none: String)            = false;
  @inline final override def contains(none: String, nada: Int) = false;

  protected[prime] def copy(src : ValueSource, root : ValueSource) : this.type = this;
  def copy() : this.type = this;

  def foreach (f: (ValueObjectField[VOType], Any) => Unit) {}
}

abstract class ValueObject_1 extends ValueObjectBase {
  // Compute indexBits as it's just zero or one property
  override def count = if (voManifest.first(self) != voManifest.first(voCompanion.empty)) 1 else 0;
  def initIndexSet   = if (voManifest.first.isInstanceOf[VOValueObjectField[_,_]]) 0 else voManifest.fieldIndexMask * count;
  def voIndexSet     = voManifest.fieldIndexMask * count;

  def srcDiff = {
    val mine = voManifest.first(self);
    if (voManifest.first(voSource,None) != mine) 1 << voManifest.lastFieldIndex else 0;
  }

  def foreach (f: (ValueObjectField[VOType], Any) => Unit) {
    val field = voManifest.first;
    if (count == 1) f(field, field(self));
  }
}

abstract class ValueObject_4(_voIndexSet0 : Int, _srcDiff0 : Int) extends ValueObjectBase {
  protected val _bits: Byte = (_voIndexSet0 & 0xF) | (_srcDiff0 << 4) toByte;
  protected def _voIndexSet = _bits & 0xF;
  
  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = (_bits >>> 4);// & 0xF;

  def initIndexSet = _voIndexSet;
}

abstract class ValueObject_8(protected val _voIndexSet : Byte, protected val _srcDiff : Byte) extends ValueObjectBase {
  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = _srcDiff;

  def initIndexSet = _voIndexSet;
}

abstract class ValueObject_16(protected val _voIndexSet : Short, protected val _srcDiff : Short) extends ValueObjectBase {
  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = _srcDiff;

  def initIndexSet = _voIndexSet;
}

abstract class ValueObject_32(protected val _voIndexSet : Int, protected val _srcDiff : Int) extends ValueObjectBase {
  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = _srcDiff;

  def initIndexSet = _voIndexSet;
}


// ------------
// VO Companion
// ------------
trait ValueObjectCompanion[T <: ValueObject] {
  val empty    : T;
  val fields   : Array[_ <: ValueObjectField[T]];
  val manifest : ValueObjectManifest[T];

  def   apply(src : ValueSource) : T  = empty.conj(src, root = src)
  def valueOf(any : Any)         : T  = any match { case vo:T => vo; case _ => apply(ValueSource(any, this)); }
  def unapply(any : Any)  : Option[T] = try Option(valueOf(any)) catch { case _ => None }
}
