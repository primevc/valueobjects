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

  type URL = java.net.URL
  type URI = java.net.URI
  type ObjectId = org.bson.types.ObjectId
  type EmailAddr = javax.mail.internet.InternetAddress

  type Date     = org.joda.time.DateMidnight
  type DateTime = org.joda.time.DateTime
  type Interval = org.joda.time.Interval

  val emptyString    = "";
  val emptyURI       = new URI("");
  val emptyURL       = new URL("http:");
  val emptyEmailAddr = new EmailAddr("",null);
  val emptyFileRef   = new FileRef("",Array[Byte]());
  val emptyObjectId  = new ObjectId(Array[Byte](0,0,0,0,0,0,0,0,0,0,0,0));
  val minDate        = new Date(0);
  val minDateTime    = new DateTime(0);
  val minInterval    = new Interval(0,0);

  sealed abstract class ValueType {
    val keyword : Keyword;
    /** True if fields of this type should be initialized lazily in ValueObjects. */
    def isLazy  : Boolean = false;
    def convert(any:Any) : Any;
    def klass   : Class[_];
  }

  object ValueTypes
  {
    import prime.types.Conversion._
    import clojure.lang.Keyword.{intern => k};

    case class Tdef       (empty:ValueObject, ref:Boolean) extends ValueType {
      require(empty != null, "empty ValueObject needed")
      //require(empty.voManifest != null, "ValueObject needs a manifest")
      //require(empty.voManifest.VOType != null, "ValueObject needs a manifest.VOType")
      //require(empty.voManifest.VOType.erasure != null, "ValueObject needs a manifest.VOType.erasure")

      val keyword          = k(empty.getClass.getPackage.getName, empty.getClass.getSimpleName.dropRight(2).intern);
      override def isLazy  = !ref;
      def convert(any:Any) = if (!ref) empty.voCompanion.valueOf(any) else VORef.valueOf(any)(empty.asInstanceOf[ValueObject with ID{type IDType = Any}]);
      def klass : Class[_] = if (!ref) empty.getClass else classOf[VORef[_]]
    }
    case class Tenum      (t: Enum) extends ValueType {
      val keyword          = k(t.getClass.getPackage.getName, t.getClass.getSimpleName);
      def convert(any:Any) = t.valueOf(any);
      def klass            = t.getClass
    }
    case class Tarray     (innerType:ValueType, min:Int=0, max:Int=Int.MaxValue) extends ValueType {
      val keyword          = k("prime.types", "Vector-of-" + innerType.keyword.sym.getName intern);
      override def isLazy  = true;
      def convert(any:Any) = Vector(any)(innerType.convert);
      def klass            = classOf[IndexedSeq[_]]
    }

    case class Tbool      (default:Boolean=false)                                                   extends ValueType { val keyword = k("prime.types", "boolean"); def convert(any:Any) = Boolean(any); def klass = java.lang.Boolean.TYPE; }
    case class Tinteger   (min:Int=Int.MinValue,  max:Int=Int.MaxValue, stride:Int=0)               extends ValueType { val keyword = k("prime.types", "integer"); def convert(any:Any) = Integer(any); def klass = java.lang.Integer.TYPE; }
    case class Tdecimal   (min:Double=Double.MinValue, max:Double=Double.MaxValue, stride:Double=0) extends ValueType { val keyword = k("prime.types", "decimal"); def convert(any:Any) = Decimal(any); def klass = java.lang .Double.TYPE; }

    case object Tdate      extends ValueType { val keyword = k("prime.types", "Date");      def convert(any:Any) = Date(any);      def klass = classOf[Date];      }
    case object Tdatetime  extends ValueType { val keyword = k("prime.types", "Date+time"); def convert(any:Any) = DateTime(any);  def klass = classOf[DateTime];  }
    case object Tinterval  extends ValueType { val keyword = k("prime.types", "Interval");  def convert(any:Any) = Interval(any);  def klass = classOf[Interval];  }
    case object Tcolor     extends ValueType { val keyword = k("prime.types", "Color");     def convert(any:Any) = RGBA(any);      def klass = classOf[RGBA];      }
    // case object Tbitmap    extends ValueType

    case object Tstring    extends ValueType { val keyword = k("prime.types", "String");    def convert(any:Any) = String(any);    def klass = classOf[String];    }
    case object Turi       extends ValueType { val keyword = k("prime.types", "URI");       def convert(any:Any) = URI(any);       def klass = classOf[URI];       }
    case object Turl       extends ValueType { val keyword = k("prime.types", "URL");       def convert(any:Any) = URL(URI(any));  def klass = classOf[URL];       }
    case object Temail     extends ValueType { val keyword = k("prime.types", "E-mail");    def convert(any:Any) = EmailAddr(any); def klass = classOf[EmailAddr]; }

    case object TobjectId  extends ValueType { val keyword = k("prime.types", "ObjectId");  def convert(any:Any) = ObjectId(any);  def klass = classOf[ObjectId];  }
    case object TfileRef   extends ValueType { val keyword = k("prime.types", "FileRef");   def convert(any:Any) = FileRef(any);   def klass = classOf[FileRef];   }
  }
}
