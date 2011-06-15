package primevc.types;
 import java.lang.Integer.parseInt
 import java.lang.Long.parseLong
 import scala.util.matching.Regex

class RGBA private[types](val rgba:Int)
{
  private[types] def this(s:String) = this(s.trim match
  {
  	case "" => 0
      // Could add color names here...
      
    case s:String if (s.indexOf('#') == 0) =>
      if (s.length <= 7)
        parseInt(s.substring(1), 16) << 8
      else
        parseLong(s.substring(1), 16).toInt

    case s:String if (s.startsWith("0x")) =>
      if (s.length <= 8)
        parseInt(s.substring(2), 16) << 8
      else
        parseLong(s.substring(2), 16).toInt
    
    case s:String =>
      parseLong(s).toInt
  })

  final def alphaPercent: Float = alpha.toFloat / 255
  final def rgb   = (rgba >>> 8)
  final def red   = (rgba & 0xFF000000) >>> 24
  final def green = (rgba &   0xFF0000) >>> 16
  final def blue  = (rgba &     0xFF00) >>>  8
  final def alpha = (rgba &       0xFF)

  lazy val toRGBString = "#%06X".format(rgb)
  lazy val toRGBAString = "#%08X".format(rgba)
  override lazy val toString = toRGBString // "0x" + rgba.toHexString.toUpperCase
  final def toInt = rgba

  override def equals(other:Any) = other match {
    case color:Int => color == this.rgba
    case color:RGBA => color.rgba == this.rgba
    case _ => super.equals(other)
  }
}

object RGBA
{
  val black = new RGBA(0)          { override lazy val toString = "0x00000000" }
  val white = new RGBA(0xFFFFFFFF) { override lazy val toString = "0xFFFFFFFF" }

  private val all_0 = new Regex("(?i)([0x#]*)")
  private val all_F = new Regex("(?i)([Fx#]*)")

  def apply(s:String): RGBA = s match {
    case all_0(_) => black
    case all_F(_) => white
    case _ => new RGBA(s)
  }
  def apply(i:Int): RGBA = i match {
    case 0 => black
    case 0xFFFFFFFF => white
    case _ => new RGBA(i)
  }
  def apply(i:Long): RGBA = apply((i & 0xFFFFFFFF).toInt);
  def apply(rgb:Int, a:Int): RGBA = apply((rgb << 8) | a)
  def apply(rgb:Int, alphaPercentage:Float): RGBA = apply(rgb, (255 * alphaPercentage).toInt)
}
