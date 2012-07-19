package prime.vo
 import prime.vo.source._
 import prime.vo.util._

trait ValueObject extends ValueSource
// with IndexedValueSource[Any]
// with NamedValueSource[Any]
{
  type VOType <: ValueObject;

  /** Hints which properties are likely different from the `voSource`.
      Could return a false positive, but _not_ a false negative. */
  protected[prime] def srcDiff  : Int;

  /** Returns:
      - A new copy with data from `src` and with `new.voSource == root`.
      - Or `this`: if `src` contains no different data or is empty.
  */
  protected[prime] def copy(src : ValueSource, root : ValueSource) : this.type;

  /** Directly get the `voIndexSet` bits that do not require realizing sub-VOs. */
  protected[prime] def lazyIndexSet : Int;

  //
  // ValueObject members
  //

  /**
    Acts like a bit-set. Every bit indicates there is a value for that index.
    Example: if (voIndexSet & (1 << 20) != 0) { ...this(20) has a value... }
  */
  def voIndexSet  : Int;
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
  final def contains(key : Symbol) : Boolean = this.contains(key.name);

  /** True if all sub-ValueObjects recursively have been initialized from their ValueSources. */
  def isRealized : Boolean;
  /** */
  def realized   : VOType;

  /**
    Conjoin `src` and `this`.

    Will return a new copy with all properties from `src` and any other properties from `this`.
    If `src` has no different values or is empty, `this` is returned.
  */
  final def    conj(src : ValueSource)         : this.type = copy(src, this.voSource);

  def         assoc(idx : Int,    value : Any) : this.type = conj(new SingleValueSource(voManifest.index(idx), value));
  def         assoc(key : String, value : Any) : this.type = conj(new SingleValueSource(voManifest.index(key), value));
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

  @inline override def contains(id  : Int) : Boolean = (voIndexSet & (1 << id)) != 0;

  final def isRealized = true;
  final def realized   = self;
}

trait NoProperties extends LeafNode {
  this : ValueObjectBase =>
//  @inline final override def containsKey(idx:Int, name:String) = false;
  @inline final override def contains(idx:  Int)            = false;
  @inline final override def contains(name: String)         = false;
  
  def voIndexSet : Int = 0;
  def srcDiff    : Int = 0;
  override def   count = 0;

  def copy       (src: ValueSource, root: ValueSource) = this;
  def foreach    (b: (ValueObjectField[ValueObject#VOType], Any) => Unit) {}
}


// ---------------
// VO Base classes
// ---------------

abstract class ValueObjectBase extends ValueObject with ClojureMapSupport {
  final def contains(name: String, orIdx: Int) : Boolean = contains(name) || contains(voManifest.index(orIdx));

//  final def apply(idx:  Int,    notFound: Any) : Any = apply(null, idx, notFound)
//  final def apply(name: String, notFound: Any) : Any = apply(name,  -1, notFound)
  def anyAt(name: String, idx: Int, notFound: Any) : Any = {
    val field = try voManifest(name) catch {
      case e:NoSuchFieldException =>
        try voManifest(idx) catch { case e:NoSuchFieldException => null }
    }
    if (field != null && (field in self)) field(self);
    else notFound;
  }

  def count = java.lang.Integer.bitCount(this.voIndexSet);

  override def toString = voManifest.VOType.erasure.getSimpleName + "(" +
    TraversableValueObject(this).map({ case (f, v) => f.symbol.name + " = " + v }).mkString(", ") +
  ")";
}

abstract class ValueObject_1 extends ValueObjectBase {
  // No indexBits, it's just zero or one property
  override def count = if (this.contains(voManifest.lastFieldIndex)) 1 else 0
  protected[prime] def lazyIndexSet = 0;
}

abstract class ValueObject_4(_voIndexSet0 : Int, _srcDiff0 : Int) extends ValueObjectBase {
  if (voSource == voCompanion.empty)
    assert(_voIndexSet0 == _srcDiff0, "When src is empty, the voIndexSet{%x} should == srcDiff{%x}".format(_voIndexSet0, _srcDiff0));

  protected val _bits: Byte = (_voIndexSet0 & 0xF) | (_srcDiff0 << 4) toByte;
  protected def _voIndexSet = _bits & 0xF;
  
  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = (_bits >>> 4);// & 0xF;

  protected[prime] def lazyIndexSet = _voIndexSet;
}

abstract class ValueObject_8(protected val _voIndexSet : Byte, protected val _srcDiff : Byte) extends ValueObjectBase {
  if (voSource == voCompanion.empty)
    assert(_voIndexSet == _srcDiff, "When src is empty, the voIndexSet{%x} should == srcDiff{%x}".format(_voIndexSet, _srcDiff));

  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = _srcDiff;

  protected[prime] def lazyIndexSet = _voIndexSet;
}

abstract class ValueObject_16(protected val _voIndexSet : Short, protected val _srcDiff : Short) extends ValueObjectBase {
  if (voSource == voCompanion.empty)
    assert(_voIndexSet == _srcDiff, "When src is empty, the voIndexSet{%x} should == srcDiff{%x}".format(_voIndexSet, _srcDiff));

  def voIndexSet : Int = _voIndexSet;
  def srcDiff    : Int = _srcDiff;

  protected[prime] def lazyIndexSet = _voIndexSet;
}

abstract class ValueObject_32(protected[prime] val lazyIndexSet : Int, val _srcDiff : Int) extends ValueObjectBase {
  if (voSource == voCompanion.empty)
    assert(lazyIndexSet == _srcDiff, "When src is empty, the voIndexSet{%x} should == srcDiff{%x}".format(lazyIndexSet, _srcDiff));

  def voIndexSet : Int = lazyIndexSet;
  def srcDiff    : Int = _srcDiff;
}


// ------------
// VO Companion
// ------------
trait ValueObjectCompanion[T <: ValueObject] {
  val empty    : T;
  val manifest : ValueObjectManifest[T];

  def   apply(src : ValueSource) : T  = empty.copy(src, root = src)
  def valueOf(any : Any)         : T  = any match { case vo:T => vo; case _ => apply(ValueSource(any, this)); }
  def unapply(any : Any)  : Option[T] = Option(valueOf(any))
}
