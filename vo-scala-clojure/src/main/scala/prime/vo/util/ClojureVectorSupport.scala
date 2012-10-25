package prime.vo.util
 import prime.types.Conversion._;
 import clojure.lang.{Var, RT, Util, ArityException, Keyword, MapEntry}
 import clojure.lang.{ASeq, ISeq, IHashEq, Counted, IPersistentCollection, IPersistentMap, IPersistentVector, PersistentVector}
 import scala.collection.JavaConversions._

/**
  Accessing a ValueObject Seq[_] field with keyword access,
   should return an instance of this class wrapping the Seq.

  This way we can have Scala and Clojure interop and in the (hopefully) near future
  use Scala's concatenation optimized Vectors.
*/
case class ScalaSeqWrapper[A](underlying : Seq[A])(implicit val to_A : Any => A, val itemType : Manifest[A]) extends java.util.AbstractList[A]
 with IPersistentVector
 with ClojureFn
 with java.util.List[A]
 with java.util.RandomAccess
 with java.lang.Comparable[IPersistentVector]
 with IHashEq
{
  // https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/APersistentVector.java
  // https://github.com/scala/scala/blob/v2.9.2/src/library/scala/collection/JavaConversions.scala

  // http://www.scala-lang.org/api/current/scala/collection/immutable/IndexedSeq.html
  // 

  def size   = underlying.length
  def count  = underlying.length
  def length = underlying.length

  override def iterator = IteratorWrapper(underlying.iterator)
  override def isEmpty = underlying.isEmpty

  def get(i : Int) = underlying(i)

  override def equals(any : Any) = any match {
    case s:ScalaSeqWrapper[_] => s.underlying == underlying;
    case v:IPersistentVector  => v.equals(this);
    case s:Seq[_]             => s == underlying;
    case _                    => false;
  }

  // IPersistentCollection
  def empty = ClojureVectorSupport.emptyCljVector;

  def equiv(any : Any): Boolean = {
    val s:Seq[_] = any match {
      case s : ScalaSeqWrapper[_] => s.underlying;
      case s :             Seq[_] => s;
      case s :  java.util.List[_] => s;
    }
    if (s.length != underlying.length) return false;

    for (i <- s.indices) if (!Util.equiv(underlying(i), s(i))) return false;
    return true;
  }

  // IPersistentVector
  def cons(any : Any) = ScalaSeqWrapper(underlying :+ to_A(any));

  // IPersistentStack
  def peek = if (underlying.length > 0) underlying.last.asInstanceOf[AnyRef] else null;
  def pop  = ScalaSeqWrapper(underlying.dropRight(1))

  // Indexed
  final def nth(index : Int) = underlying(index).asInstanceOf[AnyRef]
  final def nth(index : Int, notFound:AnyRef) = index match {
    case i if underlying.indices.contains(i) => underlying(i).asInstanceOf[AnyRef]
    case _ => notFound
  }

  // ILookup
  final def valAt(key: Any, notFound: AnyRef) = try nth(Integer(key), notFound) catch { case _:ConversionException => notFound }
  final def valAt(key: Any) = valAt(key, null)

  // Associative
  final def assoc (key: Any, value: Any) : IPersistentVector = ScalaSeqWrapper(underlying.updated(Integer(key), to_A(value)));
  final def assocN(key: Int, value: Any) : IPersistentVector = ScalaSeqWrapper(underlying.updated(key, to_A(value)));

  final def entryAt(key: Any) = {
    val k = Integer(key);
    new MapEntry(key, underlying(k))
  }
  final def containsKey(key: Any) = try underlying.indices.contains(Integer(key)) catch { case _:ConversionException => false }

  // Seqable
  final class CljSeq(i : Int, meta: IPersistentMap = null) extends ASeq(meta) with Counted
  {
    def first = underlying(i).asInstanceOf[AnyRef];
    def next  = if ((i + 1) < underlying.length) new CljSeq(i + 1) else null;

    override def count = underlying.length - i;

    def withMeta(meta : IPersistentMap) = new CljSeq(i, meta);
  }
  final class CljRSeq(i : Int, meta: IPersistentMap = null) extends ASeq(meta) with Counted
  {
    def first = underlying(i).asInstanceOf[AnyRef];
    def next  = if (i > 0) new CljRSeq(i - 1) else null;

    override def count = i + 1;

    def withMeta(meta : IPersistentMap) = new CljRSeq(i, meta);
  }
  def seq  = if (!isEmpty) new CljSeq(0) else null;
  def rseq = if (!isEmpty) new CljRSeq(underlying.length - 1) else null;

  // IFn
  override final def invoke (key: AnyRef)                   : AnyRef = nth(Integer(key))
  override final def invoke (key: AnyRef, notFound: AnyRef) : AnyRef = valAt(key, notFound)

  final          def applyTo(arglist: ISeq) = RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case 2 => invoke(arglist.first, arglist.next.first);
    case n => throwArity(n);
  }

  // Comparable
  def compareTo(v : IPersistentVector): Int = {
    if     (count < v.count) -1;
    else if(count > v.count)  1;
    else
    {
      for (i <- underlying.indices) {
        val c = Util.compare(nth(i), v.nth(i));
        if(c != 0) return c;
      }
      0;
    }
  }

  // IHashEq
  def hasheq() = {
    var hash = 1;
    for (obj <- underlying)
      hash = 31 * hash + Util.hasheq(obj);
    
    hash;
  }
}


/**
  Wraps a Clojure IPersistentVector for use as a Scala IndexedSeq[_].

  Storing a Clojure-Vector into a ValueObject should wrap it with this class.
*/
case class ClojureVectorWrapper[A](underlying : IPersistentVector)(implicit val to_A : Any => A) extends scala.collection.immutable.IndexedSeq[A] {
  def length = underlying.count
  def apply(i : Int) = to_A(underlying.nth(i))
  override def isEmpty = underlying.count == 0

  override def equals(any : Any) = any match {
    case s:ClojureVectorWrapper[_] => s.underlying == underlying;
    case v:IPersistentVector       => v.equals(underlying);
    case s:Seq[_]                  => underlying.equals(s);
    case _                         => false;
  }
}

object ClojureVectorSupport {
  final def emptyCljVector = ScalaSeqWrapper(IndexedSeq.empty)

  implicit def asScala[A]( v : IPersistentVector )(implicit to_A : Any => A, typeA : Manifest[A]) : IndexedSeq[A] = v match {
    case s @ ScalaSeqWrapper(wrapped) => if (typeA != null && s.itemType == typeA) wrapped.asInstanceOf[Seq[A]].toIndexedSeq else wrapped.map(to_A).toIndexedSeq
    case _ => ClojureVectorWrapper(v)
  }

  implicit def asClojure[A]( v : Seq[A] )(implicit to_A : Any => A, typeA : Manifest[A]) : IPersistentVector = v match {
    case ClojureVectorWrapper(wrapped) => wrapped
    case _ => ScalaSeqWrapper(v)
  }
}
