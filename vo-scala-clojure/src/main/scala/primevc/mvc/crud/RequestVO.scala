package primevc.mvc.crud
 import primevc.types._
 import primevc.core.traits._
 import primevc.utils.ConvertTo
 import org.joda.time.Interval
 import scala.collection.immutable.SortedMap

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 16-okt-2010
 * Time: 16:09:09
 * To change this template use File | Settings | File Templates.
 */

object RequestVO
{
  def apply[A <: ValueObject]
    (requestMapReverseSorted:SortedMap[String, Array[String]], skipKeys: Set[String], a:VOCompanion[A]) : A = {
    val data = requestMapReverseSorted.iterator.buffered
    doFillVO("", a, a.empty, data, skipKeys)
  }

  def apply[A <: ValueObject]
    (requestMapReverseSorted:SortedMap[String, Array[String]], skipKeys: Set[String], a:(String, VOCompanion[A])) : A = {
    val data = requestMapReverseSorted.iterator.buffered
    doFillVO(a._1, a._2, a._2.empty, data, skipKeys)
  }

  def apply[A <: ValueObject, B <: ValueObject]
    (requestMapReverseSorted:SortedMap[String, Array[String]], skipKeys: Set[String], a:(String, VOCompanion[A]), b:(String, VOCompanion[B])) : (A,B) = {
    val data = requestMapReverseSorted.iterator.buffered
    val order = a._1.compareTo(b._1)
    require(order != 0, "Keys should not be the same")

    if (order > 0)
      (doFillVO(a._1, a._2, a._2.empty, data, skipKeys),
       doFillVO(b._1, b._2, b._2.empty, data, skipKeys))
    else {
      val vo_b = doFillVO(b._1, b._2, b._2.empty, data, skipKeys)
      val vo_a = doFillVO(a._1, a._2, a._2.empty, data, skipKeys)
      (vo_a, vo_b)
    }
  }

  def apply[A <: ValueObject, B <: ValueObject, C <: ValueObject]
    (requestMapReverseSorted:SortedMap[String, Array[String]], skipKeys: Set[String], a:(String, VOCompanion[A]), b:(String, VOCompanion[B]), c:(String, VOCompanion[C])) : (A,B,C) = {
    val data = requestMapReverseSorted.iterator.buffered

    val vo_a = a._2.empty
    val vo_b = b._2.empty
    val vo_c = c._2.empty

    val v = Array((a._1, a._2, vo_a), (b._1, b._2, vo_b), (c._1, c._2, vo_c));
    v.sortBy(_._1)(Ordering.String.reverse)

    for (v <- v)
      doFillVO(v._1, v._2.asInstanceOf[VOCompanion[ValueObject]], v._3, data, skipKeys)

    (vo_a, vo_b, vo_c)
  }

  protected def doFillVO[V <: ValueObject](prefix:String, voc:VOCompanion[V], vo:V, data:BufferedIterator[(String, Array[String])], skipKeys: Set[String]): V =
  {
    import Type._

    def fillArray(innerType:Type.T, data:BufferedIterator[(String, Array[String])], indexPos:Int, closePos:Int): Array[_] =
    {
      def getIndexFromKey = data.head._1.substring(indexPos, closePos).toInt
      def nextKeyValue = {
        val (key, values) = data.next // advance iterator

        println("fillArray key: " + key + ", values: " + values)

        (key, values(values.length - 1))
      }
      val prefix = data.head._1.substring(0, indexPos)
      def itemsRemain = data.hasNext && data.head._1.startsWith(prefix)

      innerType match
      {
        case Tdef(voc, _) => // Array[ValueObject]
          var i = getIndexFromKey
          val arr = new Array[ValueObject](i + 1);
          var currentVO = voc.empty

          do
          {
            val (key, value) = nextKeyValue
            require(closePos + 2 < key.length, "Array key '"+ key +"' closePos: "+ (closePos + 2) +" < "+ key.length +" for Tdef("+ voc.getClass.getName +")")
            println("putValue  key: "+ key + ", key-substring: '" + key.substring(closePos+2) + "' value: '" + value + "'")
            voc.putValue(currentVO, key.substring(closePos+2), value)

            if (itemsRemain)
            {
              val nextIndex = getIndexFromKey
              if (nextIndex != i) {
                arr(i) = currentVO
                currentVO = voc.empty
                i = nextIndex
              }
            }
            else { // No more array items in data
              arr(i) = currentVO
              return(arr.filter(!_.empty_?))
            }

          } while (true)
          arr

        case _ => // simple values
          var i = getIndexFromKey
          val arr = new Array[String](i + 1); //prealloc array
          do {
            val (key, value) = nextKeyValue

            arr(i) = value;
            if (itemsRemain) i = getIndexFromKey
            else return(arr)

          } while (true)
          arr
      }
    }

    def process[VO <: ValueObject](prefix:String, vo:VO, voc:VOCompanion[VO], data:BufferedIterator[(String, Array[String])], keyStringOffset:Int): VO =
    {
      while (data.hasNext)
      {
        val (key, values) = data.head

        if (!key.startsWith(prefix))
          return(vo);

        if (skipKeys(key))
          data.next()
        else
        {
          val value = values(values.length - 1)
          val dot = key.indexOf('.', keyStringOffset)
          val bracketOpen = key.indexOf('[', keyStringOffset)

          if (dot - bracketOpen == 0) { // (-1  -  -1 == 0): No . and no [
            if (value.length != 0) voc.putValue(vo, key.substring(keyStringOffset), value)
            data.next()
          }
          else
          {
            val fieldName = key.substring(keyStringOffset, if (bracketOpen == -1 || dot > 0 && dot < bracketOpen) dot else bracketOpen)
            println("process field: " + fieldName)
            val fieldIndex = voc.field(fieldName)
            require(fieldIndex != -1, "Field not found: " + fieldName)
            val field = voc.field(fieldIndex)

            field.valueType match {
              case Tdef(subvoc, _) =>
                require(dot >= 0, "Not a property: "+key);
                val subvo = voc.getValue(vo, fieldIndex).asInstanceOf[ValueObject]
                voc.putValue(vo, fieldIndex, process(key.substring(0, dot+1), subvo, subvoc.asInstanceOf[VOCompanion[ValueObject]], data, dot+1))

              case Tarray(innerType,_,_) =>
                require(bracketOpen > 0 && bracketOpen < key.length - 2, "Not an array setter:" + key)
                val bracketClose = key.indexOf(']', bracketOpen + 1)
                require(bracketClose != -1);

                val v = fillArray(innerType, data, bracketOpen+1, bracketClose)
                println("fillArray '"+key+": "+v.length)
                if (v.length != 0) voc.putValue(vo, fieldIndex, v)

              case Tinterval =>
                if (value.length > 0)
                {
                  val interval:Interval = voc.getValue(vo, fieldIndex).asInstanceOf[Interval]
                  voc.putValue(vo, fieldIndex,
                      if (key.endsWith("end"))
                        new Interval(if (interval == null) 0L else interval.getStartMillis, ConvertTo.datetime(value).getMillis)
                    else
                        new Interval(ConvertTo.datetime(value).getMillis, if (interval == null) java.lang.Long.MAX_VALUE else interval.getEndMillis)
                  );
                }
                data.next()
            }
          }
        }
      }
      vo
    }

    process[V](prefix, vo, voc, data, prefix.length)
  }
}
