package prime.types;
 import prime.vo.util.ClojureFn;
 import org.msgpack.`object`.{RawType, IntegerType};
 import clojure.lang.{ILookup, ISeq, Keyword, RT};

abstract class EnumValue extends java.io.Serializable with ILookup
{
  def owner : Enum;
  val value : Int;

  /** The name of this enumeration. */
  override def toString = (getClass.getName stripSuffix "$" split '.' last) split '$' last;

  /** This hack makes it easier to compare Enums:  (if (:male gender-enum) "I'm a real boy!") */
  final def valAt(key : AnyRef, notFound : AnyRef) = if (key eq this) this else if (key match {
    case null        => false;
    case key:Keyword =>
      val s = key.sym;
      (s.getNamespace == null || s.getNamespace == this.getClass.getPackage.getName) && s.getName == toString;
    case key:Integer => value    == key;
    case key:String  => toString == key;
    case key         => (try Conversion.Integer(key) == value    catch { case _ => false }) ||
                        (try Conversion.String (key) == toString catch { case _ => false });
  }) this else null;

  final def valAt(key : AnyRef) = valAt(key, null);
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
  def  Null     :     Value = null.asInstanceOf[Value];
  val  ID       : Int;
  val  valueSet : Set[Value];

  protected def stringCatchAll(value : String): Value = throw new java.lang.NoSuchFieldException(value);
  protected def customValueOf (value : Any)    = try apply(Conversion.Integer(value)) catch { case _:Conversion.ConversionException => apply(Conversion.String(value)) }

  def       apply(value : Int)         : Value;// = values.find(_.value    == value) getOrElse(Null);
  def       apply(value : String)      : Value = valueSet.find(_.toString == value) getOrElse(stringCatchAll(value));
  final def apply(value : Long)        : Value = apply(value.toInt);
  final def apply(value : RawType)     : Value = apply(value.asString);
  final def apply(value : IntegerType) : Value = apply(value.asInt);
  final def apply(value : Keyword)     : Value = apply(value.sym.getName);
  final def apply(value : Symbol)      : Value = apply(value.name);

  def valueOf(value : Any): Value = Conversion.unpack(value) match {
    case v : Keyword     => apply(v)
    case v : org.msgpack.`object`.IntegerType => apply(v)
    case v : org.msgpack.`object`.RawType     => apply(v)
    case v : String      => apply(v)
    case v : Int         => apply(v)
    case v : Long        => apply(v)
    case v : Symbol      => apply(v)
    case v : EnumValue   => if (valueSet(v.asInstanceOf[Value])) v.asInstanceOf[Value] else apply(v.toString)
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
