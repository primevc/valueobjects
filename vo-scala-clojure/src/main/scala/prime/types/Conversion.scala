package prime.types;
 import prime.vo._;
 import org.joda.time.{ReadableInstant}
 import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
 import org.apache.commons.codec.binary.Base64;
 import org.msgpack.`object`._
 import prime.utils.msgpack.MessagePackObjectId;
 import java.text.{ParseException, DecimalFormatSymbols, DecimalFormat}
 import java.util.{Locale, Collection}
 import java.lang.Integer
 import scala.util.matching.Regex
 import scala.collection.JavaConversions

object Conversion
{
  abstract class ConversionException(msg : String) extends Exception(msg);
  object NoInputException extends ConversionException("Don't know how to convert None or null.");
  object FailureException extends ConversionException("Conversion failed.");

  @inline final def unpack(value:Any) : Any = value match {
    case null | _ : NilType | None => None
    case Some(v) => unpack(v)
    case _ => value
  }
  @inline final def to[A]( v : Any )(implicit to_A: Any => A) : A = to_A(v);

  import ClojureProtocolVars._

  // Conversion Protocol per Value type

  //  -------

  def String    (value:String)                : String = if (value == null || value.isEmpty) emptyString else value;
  def String    (value:RawType)               : String = value.asString;
  def String    (value:Number)                : String = value.toString;
  def String    (value:Integer)               : String = value.toString;
  def String    (value:Long)                  : String = value.toString;
  def String    (value:Double)                : String = value.toString;
  def String    (value:Double, format:String) : String = if (format == null) value.toString else decimalFormatter(format).format(value);
  def String    (value:URI)                   : String = value.toString;
  def String    (value:ObjectId)              : String = Base64.encodeBase64URLSafeString(value.toByteArray);
  def String    (value:Any, format:String)    : String = unpack(value) match {
    case v:String      => String(v)
    case v:RawType     => String(v)
    case v:ObjectId    => String(v)
    case v:Double      => String(v, format)
    case v:Number      => String(v)
    case v:URI         => String(v)
    case None          => throw NoInputException; //TODO: or emptyString?
    case value         => try {
      val v = if (format != null) string.invoke(value, format) else string.invoke(value);
      if (v != null) v.asInstanceOf[String] else throw FailureException;
    } catch {
      case _ => value toString;
    }
  }

  def String(value:Any) : String = String(value, null);

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

  def Boolean(value:Any) : Boolean = unpack(value) match {
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

  def Integer(value:Any) : Int = unpack(value) match {
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
    if (format == null) value.toDouble;
    else try
      decimalFormatter(format).parse(value).doubleValue
    catch {
      case e : ParseException =>
        val pipe = format.indexOf('|');
        if (pipe >= 0 && pipe < 6)
          decimalFormatter(format.substring(0,pipe+1)).parse(value).doubleValue
        else
          Decimal(value)
    }

  def Decimal(value:Any) : Double = Decimal(value, null);

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

  def RGBA(value:Any) : RGBA = unpack(value) match {
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
  def Date      (value:ReadableInstant)                     : Date = new Date(value);
  def Date      (value:java.util.Date)                      : Date = new Date(value);
  def Date      (value:Long)                                : Date = new Date(value);
  def Date      (value:Number)                              : Date = new Date(value.longValue);
  def Date      (value:String, formatter:DateTimeFormatter) : Date = formatter.parseDateTime(String(value)).toDateMidnight;
  def Date      (value:String)                              : Date = Date(value, ISODateTimeFormat.dateParser);
  def Date      (value:IntegerType)                         : Date = new Date(value.asLong);
  def Date      (value:FloatType)                           : Date = new Date(value.asLong);
  def Date      (value:RawType)                             : Date = Date(value.asString);
  def Date      (value:Any, formatter:DateTimeFormatter)    : Date = unpack(value) match {
    case v:Date            => v
    case v:java.util.Date  => Date(v)
    case v:ReadableInstant => Date(v)
    case v:Long            => Date(v)
    case v:Number          => Date(v)
    case v:String          => if (formatter == null) Date(v) else Date(v,  formatter)
    case v:IntegerType     => Date(v)
    case v:FloatType       => Date(v)
    case None              => throw NoInputException;
    case value             => val v = if (formatter == null) date.invoke(value) else date.invoke(value, formatter);
    ;                         if (v != null) v.asInstanceOf[Date] else throw FailureException;
  }

  def Date(value:Any) : Date = Date(value, null);

  //  -------

  def DateTime  (value:DateTime)                            : DateTime = value;
  def DateTime  (value:ReadableInstant)                     : DateTime = new DateTime(value);
  def DateTime  (value:java.util.Date)                      : DateTime = new DateTime(value);
  def DateTime  (value:Long)                                : DateTime = new DateTime(value);
  def DateTime  (value:Number)                              : DateTime = new DateTime(value.longValue);
  def DateTime  (value:String, formatter:DateTimeFormatter) : DateTime = formatter.parseDateTime(String(value));
  def DateTime  (value:String)                              : DateTime = DateTime(value, ISODateTimeFormat.dateTime);
  def DateTime  (value:IntegerType)                         : DateTime = new DateTime(value.asLong);
  def DateTime  (value:FloatType)                           : DateTime = new DateTime(value.asLong);
  def DateTime  (value:RawType)                             : DateTime = DateTime(value.asString);
  def DateTime  (value:Any, formatter:DateTimeFormatter)    : DateTime = unpack(value) match {
    case v:DateTime        => v
    case v:java.util.Date  => DateTime(v)
    case v:ReadableInstant => DateTime(v)
    case v:Long            => DateTime(v)
    case v:Number          => DateTime(v)
    case v:String          => if (formatter == null) DateTime(v) else DateTime(v, formatter)
    case v:IntegerType     => DateTime(v)
    case v:FloatType       => DateTime(v)
    case None              => throw NoInputException;
    case value             => val v = if (formatter == null) datetime.invoke(value) else datetime.invoke(value, formatter);
    ;                         if (v != null) v.asInstanceOf[DateTime] else throw FailureException;
  }

  def DateTime(value:Any) : DateTime = DateTime(value, null);

  //  -------

  def Interval  (value:Interval)                             : Interval = value;
  def Interval  (start:ReadableInstant, end:ReadableInstant) : Interval = new Interval(start, end);
  def Interval  (value:(_,_))                                : Interval = Interval(DateTime(value._1), DateTime(value._2));

  def Interval(value:Any) : Interval = unpack(value) match {
    case v:Interval => v
    case v:Array[_] => Interval(DateTime(v(0)), DateTime(v(1)))
    case v:(_,_)    => Interval(v)
    case None       => throw NoInputException;
    case value      => val v = interval.invoke(value); if (v != null) v.asInstanceOf[Interval] else throw FailureException;
  }

  //  -------

  def EmailAddr (value:EmailAddr) : EmailAddr = value;
  def EmailAddr (value:String)    : EmailAddr = new EmailAddr(String(value));
  def EmailAddr (value:RawType)   : EmailAddr = EmailAddr(value.asString);
  def EmailAddr (value:URI)       : EmailAddr = EmailAddr(value.getPath);
  def EmailAddr (value:URL)       : EmailAddr = EmailAddr(value.getFile);

  def EmailAddr(value:Any) : EmailAddr = unpack(value) match {
    case v:EmailAddr                                => v
    case v:String                                   => EmailAddr(v)
    case v:URI       if ("mailto" == v.getScheme)   => EmailAddr(v)
    case v:URL       if ("mailto" == v.getProtocol) => EmailAddr(v)
    case v:RawType                                  => EmailAddr(v)
    case None  => throw NoInputException;
    case value => val v = e_mail.invoke(value); if (v != null) v.asInstanceOf[EmailAddr] else throw FailureException;
  }

  //  -------

  def URI       (value:URI)       : URI = value;
  def URI       (value:String)    : URI = new URI(value.replace(" ", "%20"));
  def URI       (value:RawType)   : URI = URI(value.asString);
  def URI       (value:URL)       : URI = value.toURI;
  def URI       (value:ObjectId)  : URI = URI(String(value));

  def URI(value:Any) : URI = unpack(value) match {
    case v:URI       => v
    case v:ObjectId  => URI(v)
    case v:String    => URI(v)
    case v:URL       => URI(v)
    case v:RawType   => URI(v)
    case None        => throw NoInputException;
    case value       => val v = uri.invoke(value); if (v != null) v.asInstanceOf[URI] else throw FailureException;
  }

  //  -------

  def URL (value:URI) = if (value.getHost != null) value.toURL else throw FailureException;

  def URL (value:Any) : URL = unpack(value) match {
    case v:URL          => v
    case v:URI          => v.toURL
    case v:RawType      => URL(v.asString);
    case None           => throw NoInputException;
    case value          => val v = uri.invoke(value); if (v != null) v.asInstanceOf[URI].toURL else throw FailureException;
  }

  //  -------

  import clojure.lang.{IPersistentVector => CljIPVector}

  def Vector[T] (value:IndexedSeq[T])                                   : IndexedSeq[T] = value
  def Vector[T] (value:Array[T])                                        : IndexedSeq[T] = value
  def Vector[T] (value:java.util.List[T])                               : IndexedSeq[T] = if (value.isEmpty) IndexedSeq.empty else JavaConversions.asScalaBuffer(value).toIndexedSeq;
  def Vector[T] (value:Collection[T])                                   : IndexedSeq[T] = if (value.isEmpty) IndexedSeq.empty else JavaConversions.collectionAsScalaIterable(value).toIndexedSeq;
  def Vector[T] (value:Traversable[T])                                  : IndexedSeq[T] = value.toIndexedSeq
  def Vector[T] (value:ArrayType)       (implicit converter : Any => T) : IndexedSeq[T] = Vector(value.asArray.asInstanceOf[Array[AnyRef]])
  def Vector[T] (value:Traversable[_])  (implicit converter : Any => T) : IndexedSeq[T] = value.view.map(converter).toIndexedSeq
  def Vector[T] (value:Array[AnyRef])   (implicit converter : Any => T) : IndexedSeq[T] = {
    val v = IndexedSeq.newBuilder[T];
    v.sizeHint(value.length);
    var i = 0; while (i < value.length) {
      v += converter(value(i));
      i += 1;
    };
    v.result
  }

  def Vector[T](value:Any)(implicit converter : Any => T) : IndexedSeq[T] = unpack(value) match {
    case v:Array[AnyRef]  => Vector(v)
    case v:ArrayType      => Vector(v)
    case v:CljIPVector    => prime.vo.util.ClojureVectorSupport.asScala(v)(converter,null)
    case v:Array[_]       => v.view.map(converter).toIndexedSeq
    case v:Collection[_]  => JavaConversions.collectionAsScalaIterable(v).map(converter).toIndexedSeq
    case v:Traversable[_] => Vector(v)
    case None             => throw NoInputException;

    case value =>
      val v = vector.invoke(value, converter);
      if (v != null) v.asInstanceOf[IndexedSeq[T]] else throw FailureException;
  }

  //  -------

  implicit def vo2ref[V <: ValueObject with ID](vo : V) : VORef[V] = VORef(vo)

  //  -------

  def ObjectId  (value:ObjectId)            : ObjectId = value;
  def ObjectId  (value:MessagePackObjectId) : ObjectId = value.oid;
  def ObjectId  (value:URI)                 : ObjectId = if (value == null) throw NoInputException else ObjectId(value.toString);
  def ObjectId  (value:String)              : ObjectId = try ObjectId(Base64.decodeBase64(value)) catch { case e:IllegalArgumentException => ObjectId(value); };
  def ObjectId  (value:Array[Byte])         : ObjectId = new ObjectId(value);

  def ObjectId  (value:Any) : ObjectId = unpack(value) match {
    case v:ObjectId            => ObjectId(v)
    case v:MessagePackObjectId => ObjectId(v)
    case v:URI                 => ObjectId(v)
    case v:String              => ObjectId(v)
    case v:Array[Byte]         => ObjectId(v)
    case None                  => throw NoInputException;
    case value                 => val v = object_id.invoke(value); if (v != null) v.asInstanceOf[ObjectId] else throw FailureException;
  }
}

object ClojureProtocolVars
{
  import clojure.lang.{RT, IFn}
  RT.load("prime/types");

  val object_id = RT.`var`("prime.types", "to-ObjectId" );
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
  val vector    = RT.`var`("prime.types", "to-Vector"   );
}
