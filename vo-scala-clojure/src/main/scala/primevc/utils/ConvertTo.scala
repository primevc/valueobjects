package primevc.utils

import primevc.types._
import primevc.core.traits._
import _root_.org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
import javax.mail.internet.InternetAddress
import com.mongodb.casbah.Imports._
import org.joda.time.{DateMidnight, DateTime, Interval}
import java.util.{Locale, Date}
import java.text.{DecimalFormatSymbols, DecimalFormat}
import org.bson.BSONObject
import org.bson.types.{BasicBSONList, ObjectId}
import collection.JavaConversions
import java.net.{URISyntaxException, URL, URI}
import org.msgpack.`object`.{IntegerType, RawType}

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: Aug 24, 2010
 * Time: 11:45:56 AM
 * To change this template use File | Settings | File Templates.
 */

object ConvertTo
{
  def toArray  [T <: AnyRef      : Manifest](t:Traversable[_]) = t.map({x => ConvertTo   [T](x.asInstanceOf[AnyRef])}).filter( _ != null).toArray[T]
  def toVOArray[T <: ValueObject : Manifest](t:Traversable[_]) = t.map({x => ConvertTo.vo[T](x.asInstanceOf[AnyRef])}).filter(!_.empty_?).toArray[T]

  @inline final def unpack(value:Any) : Any = value match {
    case Some(v) => unpack(v)
    case None | null => None
    case _ => value
  }

  def apply[T <: Any : Manifest](value:Any) : T =
         if (classOf[String]  == manifest[T].erasure) string (value).asInstanceOf[T]
    else if (classOf[Integer] == manifest[T].erasure) integer(value).asInstanceOf[T]
    else if (classOf[URI]     == manifest[T].erasure) uri    (value).asInstanceOf[T]
    else throw new MatchError("ConvertTo[" + manifest[T].erasure.getName + "] not implemented")

  def vo[T <: ValueObject](value:AnyRef) : T = unpack(value) match {
    case v:T => v
  }
  def voRef[T <: ValueObjectWithID](value:AnyRef)(implicit idType:Manifest[T#IDType]) : Ref[T] = unpack(value) match {
    case v:Ref[T] => v
    case v:String if (classOf[String] == idType.erasure) => new Ref[T](string(v).asInstanceOf[T#IDType]) 
    case v:AnyRef if (idType.erasure.isAssignableFrom(v.getClass)) => new Ref[T](v.asInstanceOf[T#IDType])
    case v:AnyRef => new Ref[T](ConvertTo[T#IDType](v))
//    case _ => null.asInstanceOf[T]
  }

  /**
   * Tries to convert anything to Array of T
   * If value is a String, the string will be split on ',' ';' and '\n'
   */
	def array[T <: AnyRef : Manifest](value:Any, splitStringOn:Array[Char] = Array(',', ';', '\n')) : Array[T] = unpack(value) match {
    case v:Array[Any] =>
      if (v.getClass == manifest[Array[T]].erasure)
        v.asInstanceOf[Array[T]];
      else
        toArray[T](v);

    case v:String =>
      toArray[T](v.split(splitStringOn))

    case v:Traversable[_] =>
      toArray[T](v)
    case v:BasicDBList =>
      val s = JavaConversions.asScalaSet(v.keySet).iterator.map({ k =>
        val x = v.get(k.asInstanceOf[String]);
      }).toTraversable
      toArray[T](s)

    case _ => throw new Exception("Don't know what to do with: " + value.toString)
  }
  
  def voArray[T <: ValueObject : Manifest](value:Any, splitStringOn:Array[Char] = Array(',', ';', '\n')) : Array[T] = unpack(value) match
  {
    case v:Array[_] =>
      if (v.getClass == manifest[Array[T]].erasure)
        v.asInstanceOf[Array[T]];
      else
        toVOArray[T](v);

    case v:String => toVOArray[T](v.split(splitStringOn))
    case v:Traversable[_] => toVOArray[T](v)
    // throw "Don't know what to do with: " + value.toString
  }

  def fileRef			(value:Any) : FileRef = unpack(value) match {
    case v:FileRef => v
    case s:String => FileRef(s)
  }

  def rgba			(value:Any) : RGBA = unpack(value) match {
    case v:RGBA => v
    case s:String => RGBA(s)
    case i:Int => RGBA(i)
  }

  def uri           (value:Any) : URI = unpack(value) match {
    case v:URI => v
    case v:URL => v.toURI
    case v:String => uri(v)
    case None => null
    case _ => new URI(value.toString)
  }
  def uri (v:String) = if (v == null || v.isEmpty) null else try { new URI(v) } catch { case e:URISyntaxException => null }

  def email         (value:Any) : InternetAddress = unpack(value) match {
    case v:InternetAddress => v
    case v:URI if ("mailto" == v.getScheme)   => new InternetAddress(v.getRawSchemeSpecificPart)
    case v:URL if ("mailto" == v.getProtocol) => new InternetAddress(v.getFile)
    case v:String => new InternetAddress(v)
    case None => null
  }

  def interval      (value:Any) : Interval = unpack(value) match {
    case v:Interval => v
    case v:Array[_] => new Interval(datetime(v(0)), datetime(v(1)));
    case v:DBObject if (v.get("s") != null && v.get("e") != null) => new Interval(datetime(v.get("s")), datetime(v.get("e")));
    case None => null
  }

  def uniqueID      (value:Any) : ObjectId = unpack(value) match {
    case v:ObjectId => v
    case v:String => new ObjectId(v)
    case v:Array[Byte] => new ObjectId(v)
    case None => null
//    case _ => new ObjectId(value.toString)
  }

  def string        (value:Any) : String = unpack(value) match {
    case v:String => string(v)
    case v:RawType => string(v)
    case v:Array[String] => v.mkString(", ")
    case None => null
    case _ => value.toString
  }
  def string        (value:String) : String = if (value == null || value.isEmpty) null else value
  def string        (value:Double, format:String) = decimalFormatter(format).format(value)
  def string        (value:RawType) : String = value.asString

  def integer       (value:Any) : Int = unpack(value) match {
    case v:Int => v
    case v:Number => integer(v)
    case v:IntegerType => integer(v)
    case v:String => integer(v)
    case _ => 0
  }
  def integer       (value:String) : Int = {
    val v = value.trim
    if (v.isEmpty) 0 else v.toInt
  }
  def integer       (value:java.lang.Number) : Int = if (value == null) 0 else value.intValue
  def integer       (value:IntegerType) : Int = value.asInt

  def long          (value:String) : Long = {
    val v = value.trim
    if (v.isEmpty) 0 else v.toLong
  }

  def decimal       (value:Any, format:String = null) : Double = unpack(value) match {
    case v:Double => v
    case v:Number => v.doubleValue
    case v:String => decimal(v)
    case _ => Double.NaN
  }
  def decimal       (value:String) : Double = value.trim match {
    case "" => Double.NaN
    case v:String => v.toDouble
  }
  def decimal       (value:java.lang.Double) : Double = if (value == null) 0 else value.doubleValue

  protected def decimalFormatter(format:String) =
  {
    val pipe = format.indexOf('|')
    if (pipe == 0) new DecimalFormat(format.substring(1))
    else if (pipe >= 0 && pipe < 6)
    {
      val locale = format.substring(0, pipe)
      val formatStr = format.substring(pipe+1)
      new DecimalFormat(formatStr, new DecimalFormatSymbols(new Locale(locale)))
    }
    else new DecimalFormat(format)
  }
  def decimal       (value:String, format:String) : Double = decimalFormatter(format).parse(value).doubleValue

  def date          (value:Any, formatter:DateTimeFormatter = ISODateTimeFormat.dateParser) : DateMidnight = unpack(value) match {
    case v:DateMidnight => v
    case v:Date => new DateMidnight(v)
    case v:Long => new DateMidnight(v)
    case v:Number => new DateMidnight(v.longValue)
    case v:String => formatter.parseDateTime(v).toDateMidnight
    case None => null
    case _ => new DateMidnight(value)
  }

  def datetime      (value:Any, formatter:DateTimeFormatter = ISODateTimeFormat.dateTime) : DateTime = unpack(value) match {
    case v:DateTime => v
    case v:Date => new DateTime(v)
    case v:Long => new DateTime(v)
    case v:Number => new DateTime(v.longValue)
    case v:String => if (v.isEmpty) null else formatter.parseDateTime(v)
    case None => null
    case _ => new DateTime(value)
  }

  def boolean       (value:Any) : Boolean = unpack(value) match {
    case v:Boolean => v
    case v:String => v.trim.toUpperCase match { case "TRUE" | "1" | "YES" => true; case _ => false }
    case v:Number => v.intValue > 0
//    case None => false
//    case _ => value != null
    case _ => false
  }
}
