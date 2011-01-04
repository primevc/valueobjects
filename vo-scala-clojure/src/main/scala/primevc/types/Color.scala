package primevc.types;
 import java.lang.Integer.parseInt
 import java.lang.Long.parseLong
 import util.matching.Regex

class RGBA private[types](val argb:Int)
{
  private[types] def this(s:String) = this(s.trim match
  {
	case "" => 0
    case s:String if (s.indexOf('#') == 0) =>
      if (s.length <= 8)
        parseInt(s.substring(1), 16)
      else
        parseLong(s.substring(1), 16).toInt

    case s:String if (s.startsWith("0x")) =>
      if (s.length <= 9)
        parseInt(s.substring(2), 16)
      else
        parseLong(s.substring(2), 16).toInt
    
    case s:String =>
      // Could add color names here...
      parseLong(s).toInt
  })

  def alpha = (argb & 0xFF000000) >>> 24
  def red   = (argb &   0xFF0000) >>> 16
  def green = (argb &     0xFF00) >>>  8
  def blue  = (argb &       0xFF)

  lazy val toRGBString = "#%06X".format(argb)
  override lazy val toString = "0x" + argb.toHexString.toUpperCase
  final def toInt = argb

  override def equals(other:Any) = other match {
    case color:Int => color == this.argb
    case color:RGBA => color.argb == this.argb
    case _ => super.equals(other)
  }
}

object RGBA
{
  val black = new RGBA(0)        { override lazy val toString = "0x000000" }
  val white = new RGBA(0xFFFFFF) { override lazy val toString = "0xFFFFFF" }

  private val all_0 = new Regex("(?i)([0x#]*)")
  private val all_F = new Regex("(?i)([Fx#]*)")

  def apply(s:String): RGBA = s match {
    case all_0(_) => black
    case all_F(_) => white
    case _ => new RGBA(s)
  }
  def apply(i:Int): RGBA = i match {
    case 0 => black
    case 0xFFFFFF => white
    case _ => new RGBA(i)
  }
}
