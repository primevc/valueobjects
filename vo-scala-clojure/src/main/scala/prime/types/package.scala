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
  def voManifest[V <: ValueObject](implicit manifest : ValueObjectManifest[V]) = manifest;

  type URL = org.apache.commons.httpclient.URI
  type URI = org.apache.commons.httpclient.URI
  type ObjectId = org.bson.types.ObjectId
  type EmailAddr = javax.mail.internet.InternetAddress

  type Date     = org.joda.time.DateMidnight
  type DateTime = org.joda.time.DateTime
  type Interval = org.joda.time.Interval
  
  val emptyString    = "";
  val emptyURI       = new URI("");
  val emptyEmailAddr = new EmailAddr("",null);
  val emptyFileRef   = new FileRef("",null);
  val minDate        = new Date(0);
  val minDateTime    = new DateTime(0);
  val minInterval    = new Interval(0,0);

  sealed abstract class ValueType {
    val keyword : Keyword;
    /** True if fields of this type should be initialized lazily in ValueObjects. */
    def isLazy  : Boolean = false;
    def convert(any:Any) : Any;
  }

  object ValueTypes
  {
    import prime.types.Conversion._
    import clojure.lang.Keyword.{intern => k};

    case class Tdef       (vo:ValueObjectCompanion[_ <: ValueObject], ref:Boolean) extends ValueType {
      val keyword          = k(vo.getClass.getPackage.getName, vo.manifest.VOType.erasure.getSimpleName);
      override def isLazy  = true;
      def convert(any:Any) = vo.valueOf(any);
    }
    case class Tenum      (t: Enum) extends ValueType {
      val keyword          = k(t.getClass.getPackage.getName, t.getClass.getSimpleName);
      def convert(any:Any) = t.valueOf(any);
    }
    case class Tarray     (innerType:ValueType, min:Int=0, max:Int=Int.MaxValue) extends ValueType {
      val keyword          = k("prime.types", "Vector-of-" + innerType.keyword.sym.getName);
      override def isLazy  = true;
      def convert(any:Any) = Vector(any)(innerType.convert);
    }

    case class Tbool      (default:Boolean=false)                                                   extends ValueType { val keyword = k("prime.types", "boolean"); def convert(any:Any) = Boolean(any); }
    case class Tinteger   (min:Int=Int.MinValue,  max:Int=Int.MaxValue, stride:Int=0)               extends ValueType { val keyword = k("prime.types", "integer"); def convert(any:Any) = Integer(any); }
    case class Tdecimal   (min:Double=Double.MinValue, max:Double=Double.MaxValue, stride:Double=0) extends ValueType { val keyword = k("prime.types", "decimal"); def convert(any:Any) = Decimal(any); }

    case object Tdate      extends ValueType { val keyword = k("prime.types", "Date");      def convert(any:Any) = Date(any); }
    case object Tdatetime  extends ValueType { val keyword = k("prime.types", "Date+time"); def convert(any:Any) = DateTime(any); }
    case object Tinterval  extends ValueType { val keyword = k("prime.types", "Interval");  def convert(any:Any) = Interval(any); }
    case object Tcolor     extends ValueType { val keyword = k("prime.types", "Color");     def convert(any:Any) = RGBA(any); }
    // case object Tbitmap    extends ValueType

    case object Tstring    extends ValueType { val keyword = k("prime.types", "String");    def convert(any:Any) = String(any); }
    case object Turi       extends ValueType { val keyword = k("prime.types", "URI");       def convert(any:Any) = URI(any); }
    case object Turl       extends ValueType { val keyword = k("prime.types", "URL");       def convert(any:Any) = URL(URI(any)); }
    case object Temail     extends ValueType { val keyword = k("prime.types", "E-mail");    def convert(any:Any) = EmailAddr(any); }

    case object TuniqueID  extends ValueType { val keyword = k("prime.types", "ID");        def convert(any:Any) = ObjectId(any); }
    case object TfileRef   extends ValueType { val keyword = k("prime.types", "FileRef");   def convert(any:Any) = FileRef(any); }
  }
}
