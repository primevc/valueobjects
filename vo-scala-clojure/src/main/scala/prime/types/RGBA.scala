package prime.types;
 import java.lang.Integer.parseInt
 import java.lang.Long.parseLong
 import scala.util.matching.Regex

class RGBA private[prime](val rgba:Int)
{
  private[prime] def this(s:String) = this(s.trim match
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

object Colors
{
  val black = new RGBA(0)
  val white = new RGBA(0xFFFFFFFF)
}
