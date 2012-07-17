package prime.types

import prime.utils.ConvertTo

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 28-12-10
 * Time: 15:02
 * To change this template use File | Settings | File Templates.
 */
trait Enum
{
  this: scala.Enumeration =>

  type EValue

  val Null: EValue
  def fromValue(v : Int): EValue
  def fromString(v : String): EValue

  def convert(value : Any): EValue = ConvertTo.unpack(value) match {
    case v : String => fromString(v)
    case v : Int    => fromValue(v)
    case v : org.msgpack.`object`.IntegerType => fromValue(v.asInt)
    case v : EValue => v
  }
}
