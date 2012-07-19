package prime;
 import prime.vo._;
 import clojure.lang.{Keyword}

/**
 * 
 * 
 * Date: 20-06-11
 * Time: 17:25
 * @author Danny Wilson
 */
package object types
{
  val emptyString = "";

  def voManifest[V <: ValueObject](implicit manifest : ValueObjectManifest[V]) = manifest;

  type URI = org.apache.commons.httpclient.URI
  type ObjectId = org.bson.types.ObjectId
  type EmailAddr = javax.mail.internet.InternetAddress

  type Date     = org.joda.time.DateMidnight
  type DateTime = org.joda.time.DateTime
  type Interval = org.joda.time.Interval
  
  sealed abstract class ValueType {
    val keyword : Keyword;
  }

  object ValueTypes
  {
    import clojure.lang.Keyword.{intern => k};

    case class Tdef       (vo:ValueObjectCompanion[_ <: ValueObject], ref:Boolean) extends ValueType { val keyword = k(vo.getClass.getPackage.getName, vo.manifest.VOType.erasure.getSimpleName) }
    // case class Tenum      (t: Enumeration) extends ValueType
    // case class Tarray     (innerType:T,   min:Int=0,  max:Int=Int.MaxValue)  extends ValueType

    case class Tbool      (default:Boolean=false)                                                   extends ValueType { val keyword = k("prime.types", "boolean") }
    case class Tinteger   (min:Int=Int.MinValue,  max:Int=Int.MaxValue, stride:Int=0)               extends ValueType { val keyword = k("prime.types", "integer") }
    case class Tdecimal   (min:Double=Double.MinValue, max:Double=Double.MaxValue, stride:Double=0) extends ValueType { val keyword = k("prime.types", "decimal") }

    case object Tdate      extends ValueType { val keyword = k("prime.types", "Date") }
    case object Tdatetime  extends ValueType { val keyword = k("prime.types", "Date+time") }
    case object Tinterval  extends ValueType { val keyword = k("prime.types", "Interval") }
    case object Tcolor     extends ValueType { val keyword = k("prime.types", "Color") }
    // case object Tbitmap    extends ValueType

    case object Tstring    extends ValueType { val keyword = k("prime.types", "String") }
    case object Turi       extends ValueType { val keyword = k("prime.types", "URI") }
    case object Turl       extends ValueType { val keyword = k("prime.types", "URL") }
    case object Temail     extends ValueType { val keyword = k("prime.types", "E-mail") }

    case object TuniqueID  extends ValueType { val keyword = k("prime.types", "ID") }
    case object TfileRef   extends ValueType { val keyword = k("prime.types", "FileRef") }
  }
}
