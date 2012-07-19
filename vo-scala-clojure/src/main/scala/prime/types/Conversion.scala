package prime.types;
 import prime.vo._;
 import prime.vo.{ValueObjectCompanion => IVOC};
 import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
 import org.msgpack.`object`._
 import java.text.{ParseException, DecimalFormatSymbols, DecimalFormat}
 import java.util.{Locale}
 import scala.util.matching.Regex

object Conversion
{
  abstract class ConversionException(msg : String) extends Exception(msg);
  object NoInputException extends ConversionException("Don't know how to convert None or null.");
  object FailureException extends ConversionException("Conversion failed.");

  @inline final def unpack(value:Any) : Any = value match {
    case Some(v) => unpack(v)
    case None | null | _ : NilType => None
    case _ => value
  }
  @inline final def to[A]( v : Any )(implicit to_A: Any => A) : A = to_A(v);

  import ClojureProtocolVars._

  // Conversion Protocol per Value type
  def UniqueID  (value:Any) : ObjectId = unique_id.invoke(value)           .asInstanceOf[ObjectId]

  //  -------

  def String    (value:String)                : String = if (value == null || value.isEmpty) emptyString else value;
  def String    (value:RawType)               : String = value.asString;
  def String    (value:Double, format:String) : String = if (format == null) value.toString else decimalFormatter(format).format(value);
  def String    (value:URI)                   : String = value.getEscapedURIReference;
  def String    (value:Any, format:String)    : String = unpack(value) match {
    case v:String      => String(v)
    case v:RawType     => String(v)
    case v:Double      => String(v, format)
    case v:URI         => String(v)
    case None          => throw NoInputException; //TODO: or emptyString?
    case value         => val v = if (format != null) string.invoke(value, format) else string.invoke(value); if (v != null) v.asInstanceOf[String] else throw FailureException;
  }

  implicit def String(value:Any) : String = String(value, null);

  //  -------

  def Boolean   (value:Boolean)     : Boolean = value;
  def Boolean   (value:String)      : Boolean =
    if (value != null) value.trim.toLowerCase match {
      case "true"  | "1" | "yes" =>  true;
      case "false" | "0" | "no"  => false;
      case _ => throw FailureException
    } else      throw FailureException;

  def Boolean   (value:Number)      : Boolean = if (value != null) value.intValue > 0 else throw FailureException;
  def Boolean   (value:BooleanType) : Boolean = value.asBoolean;

  implicit def Boolean(value:Any) : Boolean = unpack(value) match {
    case v:Boolean     => v
    case v:String      => Boolean(v)
    case v:Number      => Boolean(v)
    case v:BooleanType => Boolean(v)
    case None          => throw NoInputException;
    case value         => val v = boolean.invoke(value); if (v != null) v.asInstanceOf[Boolean] else throw FailureException;
  }

  //  -------

  def Integer   (value:Int)         : Int = value;
  def Integer   (value:String)      : Int = { val v = value.trim; if (!v.isEmpty && v != "NaN") v.toInt; else throw FailureException }
  def Integer   (value:Number)      : Int = if (value != null) value.intValue else throw FailureException;
  def Integer   (value:IntegerType) : Int = value.asInt;
  def Integer   (value:Long)        : Int = value.toInt;

  implicit def Integer(value:Any) : Int = unpack(value) match {
    case v:Int         => v
    case v:Long        => Integer(v)
    case v:Number      => Integer(v)
    case v:String      => Integer(v)
    case v:IntegerType => Integer(v)
    case None          => throw NoInputException;
    case value         => val v = integer.invoke(value); if (v != null) v.asInstanceOf[Int] else throw FailureException;
  }

  //  -------

  def Decimal   (value:Double)                : Double = value;
  def Decimal   (value:String)                : Double = { val v = value.trim; if (!v.isEmpty) v.toDouble else throw FailureException; }
  def Decimal   (value:Number)                : Double = if (value != null) value.doubleValue else throw FailureException;
  def Decimal   (value:FloatType)             : Double = value.asDouble;
  def Decimal   (value:Any, format:String)    : Double = unpack(value) match {
    case v:Double      => v
    case v:Number      => Decimal(v)
    case v:String      => Decimal(v, format)
    case v:FloatType   => Decimal(v)
    case None          => throw NoInputException;
    case value         => val v = if (format != null) decimal.invoke(value, format) else decimal.invoke(value); if (v != null) v.asInstanceOf[Double] else throw FailureException;
  }
  def Decimal   (value:String, format:String) : Double =
    try
      decimalFormatter(format).parse(value).doubleValue
    catch {
      case e : ParseException =>
        val pipe = format.indexOf('|');
        if (pipe >= 0 && pipe < 6)
          decimalFormatter(format.substring(0,pipe+1)).parse(value).doubleValue
        else
          Decimal(value)
    }

  implicit def Decimal(value:Any) : Double = Decimal(value, null);

  protected def decimalFormatter(format:String) =
  {
    val pipe = format.indexOf('|')
    val df  = if (pipe == 0)
      new DecimalFormat(format.substring(1))
    else if (pipe >= 0 && pipe < 6)
       decimalLocaleFormatter(format.substring(0, pipe), if (pipe == format.length - 1) null else format.substring(pipe+1))
    else
        new DecimalFormat(format)

    df.setDecimalSeparatorAlwaysShown(false);
    df
  }

  protected def decimalLocaleFormatter(locale:String, format:String) =
  {
    val df = if (format == null) new DecimalFormat() else new DecimalFormat(format);
    df.setDecimalFormatSymbols(new DecimalFormatSymbols(new Locale(locale)))
    df
  }

  //  -------

  def RGBA      (value:RGBA)                     : RGBA = value;

  private val all_0 = new Regex("(?i)([0x#]*)")
  private val all_F = new Regex("(?i)([Fx#]*)")

  def RGBA      (value:String)                   : RGBA = value match {
    case all_0(_)   => Colors.black
    case all_F(_)   => Colors.white
    case _          => new RGBA(value)
  }
  def RGBA      (value:Int)                      : RGBA = value match {
    case 0          => Colors.black
    case 0xFFFFFFFF => Colors.white
    case _          => new RGBA(value)
  }
  def RGBA      (value:Number)                   : RGBA = RGBA(Integer(value));
  def RGBA      (value:IntegerType)              : RGBA = RGBA(value.asInt);
  def RGBA      (value:Long)                     : RGBA = RGBA((value & 0xFFFFFFFF).toInt);
  def RGBA      (rgb:Int, a:Int)                 : RGBA = RGBA((rgb << 8) | a);
  def RGBA      (rgb:Int, alphaPercentage:Float) : RGBA = RGBA(rgb, (255 * alphaPercentage).toInt);

  implicit def RGBA(value:Any) : RGBA = unpack(value) match {
    case v:RGBA        => v
    case v:Int         => RGBA(v)
    case v:Long        => RGBA(v)
    case v:Number      => RGBA(v)
    case v:String      => RGBA(v)
    case v:IntegerType => RGBA(v)
    case None          => throw NoInputException;
    case value         => val v = rgba.invoke(value); if (v != null) v.asInstanceOf[RGBA] else throw FailureException;
  }

  //  -------

  def Date      (value:Date)                                : Date = value;
  def Date      (value:java.util.Date)                      : Date = new Date(value);
  def Date      (value:Long)                                : Date = new Date(value);
  def Date      (value:Number)                              : Date = new Date(value.longValue);
  def Date      (value:String, formatter:DateTimeFormatter) : Date = formatter.parseDateTime(String(value)).toDateMidnight;
  def Date      (value:IntegerType)                         : Date = new Date(value.asLong);
  def Date      (value:FloatType)                           : Date = new Date(value.asLong);
  def Date      (value:RawType)                             : Date = Date(value.asString);
  def Date      (value:Any, formatter:DateTimeFormatter)    : Date = unpack(value) match {
    case v:Date           => v
    case v:java.util.Date => Date(v)
    case v:Long           => Date(v)
    case v:Number         => Date(v)
    case v:String         => Date(v, formatter)
    case v:IntegerType    => Date(v)
    case v:FloatType      => Date(v)
    case None             => throw NoInputException;
    case value            => val v = if (formatter != null) date.invoke(value, formatter) else date.invoke(value); if (v != null) v.asInstanceOf[Date] else throw FailureException;
  }

  implicit def Date(value:Any) : Date = Date(value, ISODateTimeFormat.dateParser);

  //  -------

  def DateTime  (value:DateTime)                            : DateTime = value;
  def DateTime  (value:java.util.Date)                      : DateTime = new DateTime(value);
  def DateTime  (value:Long)                                : DateTime = new DateTime(value);
  def DateTime  (value:Number)                              : DateTime = new DateTime(value.longValue);
  def DateTime  (value:String, formatter:DateTimeFormatter) : DateTime = formatter.parseDateTime(String(value));
  def DateTime  (value:IntegerType)                         : DateTime = new DateTime(value.asLong);
  def DateTime  (value:FloatType)                           : DateTime = new DateTime(value.asLong);
  def DateTime  (value:RawType)                             : DateTime = DateTime(value.asString);
  def DateTime  (value:Any, formatter:DateTimeFormatter)    : DateTime = unpack(value) match {
    case v:DateTime       => v
    case v:java.util.Date => DateTime(v)
    case v:Long           => DateTime(v)
    case v:Number         => DateTime(v)
    case v:String         => DateTime(v, formatter)
    case v:IntegerType    => DateTime(v)
    case v:FloatType      => DateTime(v)
    case None             => throw NoInputException;
    case value            => val v = if (formatter != null) datetime.invoke(value, formatter) else datetime.invoke(value); if (v != null) v.asInstanceOf[DateTime] else throw FailureException;
  }

  implicit def DateTime(value:Any) : DateTime = DateTime(value, ISODateTimeFormat.dateParser);

  //  -------

  def Interval  (value:Interval)               : Interval = value;
  def Interval  (start:DateTime, end:DateTime) : Interval = new Interval(start, end);
  def Interval  (value:(_,_))                  : Interval = Interval(DateTime(value._1), DateTime(value._2));

  implicit def Interval(value:Any) : Interval = unpack(value) match {
    case v:Interval => v
    case v:Array[_] => Interval(DateTime(v(0)), DateTime(v(1)))
    case v:(_,_)    => Interval(v)
    case None       => throw NoInputException;
    case value      => val v = interval.invoke(value); if (v != null) v.asInstanceOf[Interval] else throw FailureException;
  }

  //  -------

  def EmailAddr (value:EmailAddr)    : EmailAddr = value;
  def EmailAddr (value:String)       : EmailAddr = new EmailAddr(String(value));
  def EmailAddr (value:RawType)      : EmailAddr = EmailAddr(value.asString);
  def EmailAddr (value:URI)          : EmailAddr = EmailAddr(value.getPath);
  def EmailAddr (value:java.net.URI) : EmailAddr = EmailAddr(value.getPath);
  def EmailAddr (value:java.net.URL) : EmailAddr = EmailAddr(value.getFile);

  implicit def EmailAddr(value:Any) : EmailAddr = unpack(value) match {
    case v:EmailAddr                                   => v
    case v:String                                      => EmailAddr(v)
    case v:URI          if ("mailto" == v.getScheme)   => EmailAddr(v)
    case v:java.net.URI if ("mailto" == v.getScheme)   => EmailAddr(v)
    case v:java.net.URL if ("mailto" == v.getProtocol) => EmailAddr(v)
    case v:RawType                                     => EmailAddr(v)
    case None  => throw NoInputException;
    case value => val v = e_mail.invoke(value); if (v != null) v.asInstanceOf[EmailAddr] else throw FailureException;
  }

  //  -------

  def URI       (value:URI)          : URI = value;
  def URI       (value:String)       : URI = new URI(String(value));
  def URI       (value:RawType)      : URI = URI(value.asString);
  def URI       (value:java.net.URI) : URI = URI(value.toString);
  def URI       (value:java.net.URL) : URI = URI(value.toString);

  implicit def URI(value:Any) : URI = unpack(value) match {
    case v:URI          => v
    case v:String       => URI(v)
    case v:java.net.URI => URI(v)
    case v:java.net.URL => URI(v)
    case v:RawType      => URI(v)
    case None           => throw NoInputException;
    case value          => val v = uri.invoke(value); if (v != null) v.asInstanceOf[URI] else throw FailureException;
  }

  //  -------

  def FileRef   (value:FileRef)      : FileRef = value;
  def FileRef   (value:Array[Byte])  : FileRef = new FileRef(null, value);
  def FileRef   (value:String)       : FileRef = new FileRef(String(value), null);
  def FileRef   (value:RawType)      : FileRef = FileRef(value.asString);
  def FileRef   (value:URI)          : FileRef = FileRef(value.toString);
  def FileRef   (value:java.net.URI) : FileRef = FileRef(value.toString);
  def FileRef   (value:java.net.URL) : FileRef = FileRef(value.toString);

  def FileRef   (value:Any) : FileRef = unpack(value) match {
    case v:FileRef      => v
    case v:Array[Byte]  => FileRef(v)
    case v:String       => FileRef(v)
    case v:URI          => FileRef(v)
    case v:java.net.URI => FileRef(v)
    case v:java.net.URL => FileRef(v)
    case v:RawType      => FileRef(v)
    case None           => throw NoInputException;
    case value          => val v = file_ref.invoke(value); if (v != null) v.asInstanceOf[FileRef] else throw FailureException;
  }

  //  -------

  implicit def vo2ref[V <: ValueObject with ID](vo : V)                              : VORef[V] = new VORefImpl(vo._id, vo);
  implicit def id2ref[V <: ValueObject with ID](id : V#IDType)(implicit V : IVOC[V]) : VORef[V] = new VORefImpl(id, V.empty);

  implicit def VORef [V <: ValueObject with ID](value : Any)  (implicit V : IVOC[V], vo_IDType_converter : Any => V#IDType) : VORef[V] = unpack(value) match {
    case V(value) => vo2ref(value)
    case None     => throw NoInputException;
    case value    => try id2ref(vo_IDType_converter(value))(V) catch { case FailureException => vo_ref.invoke(value, V); }
  }
}

object ClojureProtocolVars
{
  import clojure.lang.{RT, IFn}
  RT.loadResourceScript("prime/vo/valuetype.clj");

  val unique_id = RT.`var`("prime.types", "to-UniqueID" );
  val string    = RT.`var`("prime.types", "to-String"   );
  val boolean   = RT.`var`("prime.types", "to-Boolean"  );
  val integer   = RT.`var`("prime.types", "to-Integer"  );
  val decimal   = RT.`var`("prime.types", "to-Decimal"  );
  val rgba      = RT.`var`("prime.types", "to-RGBA"     );
  val date      = RT.`var`("prime.types", "to-Date"     );
  val datetime  = RT.`var`("prime.types", "to-DateTime" );
  val interval  = RT.`var`("prime.types", "to-Interval" );
  val e_mail    = RT.`var`("prime.types", "to-EmailAddr");
  val uri       = RT.`var`("prime.types", "to-URI"      );
  val file_ref  = RT.`var`("prime.types", "to-FileRef"  );
  val vo_ref    = RT.`var`("prime.types", "to-VORef"    );
}
