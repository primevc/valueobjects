package prime.types
 import prime.vo.mutable._

object Type
{
  sealed abstract class T // Sealed tells Scala nothing outside of this object (Type) may possibly extends T

  case class Tdef       (vo:VOCompanion[_ <: ValueObject], ref:Boolean) extends T
  case class Tenum      (t: Enumeration) extends T
  case class Tarray     (innerType:T,   min:Int=0,  max:Int=Int.MaxValue)  extends T

  case class Tbool      (default:Boolean=false)  extends T
  case class Tinteger   (min:Int=Int.MinValue,  max:Int=Int.MaxValue, stride:Int=0)  extends T
  case class Tdecimal   (min:Double=Double.MinValue, max:Double=Double.MaxValue, stride:Double=0)  extends T

  case object Tdate      extends T
  case object Tdatetime  extends T
  case object Tinterval  extends T
  case object Tcolor     extends T
  case object Tbitmap    extends T

  case object Tstring    extends T
  case object Turi       extends T
  case object Turl       extends T
  case object Temail     extends T

  case object TuniqueID  extends T
  case object TfileRef   extends T
}

case class Field(name:Symbol, valueType:Type.T, required:Boolean = false)
