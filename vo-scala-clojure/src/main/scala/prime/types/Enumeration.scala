package prime.types;
 import prime.vo.util.ClojureFn;
 import clojure.lang.{ISeq, RT};

abstract class EnumValue
{
  def owner : Enum;
  val value : Int;

  /** The name of this enumeration. */
  override def toString = (getClass.getName stripSuffix "$" split '.' last) split '$' last;
}

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 28-12-10
 * Time: 15:02
 * To change this template use File | Settings | File Templates.
 */
abstract class Enum extends ClojureFn
{
  type Value <: EnumValue;
  def  Null   :     Value = null.asInstanceOf[Value];
  val  ID     : Int;
  val  values : Set[Value];

  protected def stringCatchAll(value : String): Value = throw new java.lang.NoSuchFieldException(value);
  protected def customValueOf (value : Any)    = try apply(Conversion.Integer(value)) catch { case _:Conversion.ConversionException => apply(Conversion.String(value)) }

  def       apply(value : Int)         : Value = values.find(_.value    == value) getOrElse(Null);
  def       apply(value : String)      : Value = values.find(_.toString == value) getOrElse(stringCatchAll(value));
  final def apply(value : Long)        : Value = apply(value.toInt);
  final def apply(value : org.msgpack.`object`.RawType)     : Value = apply(value.asString);
  final def apply(value : org.msgpack.`object`.IntegerType) : Value = apply(value.asInt);
  final def apply(value : clojure.lang.Keyword)     : Value = apply(value.sym.getName);
  final def apply(value : Symbol)      : Value = apply(value.name);

  def valueOf(value : Any): Value = Conversion.unpack(value) match {
    case v : EnumValue   => if (values(v.asInstanceOf[Value])) v.asInstanceOf[Value] else apply(v.toString)
    case v : String      => apply(v)
    case v : clojure.lang.Keyword     => apply(v)
    case v : Int         => apply(v)
    case v : Long        => apply(v)
    case v : org.msgpack.`object`.IntegerType => apply(v)
    case v : org.msgpack.`object`.RawType     => apply(v)
    case v : Symbol      => apply(v)
    case None            => throw Conversion.NoInputException;
    case v               => customValueOf(v)
  }

  def unapply(any : Any) : Option[Value] = try Option(valueOf(any)) catch { case _:Conversion.ConversionException => None }

  //
  // Clojure IFn
  //
  override def invoke(any : AnyRef) : AnyRef = valueOf(any)

  final def applyTo (arglist: ISeq) = RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case n => throwArity(n);
  }
}
