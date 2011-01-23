package primevc.mvc.crud

import ru.circumflex.core._
import ru.circumflex.web._
import xml.{Attribute, Text, Elem, NodeSeq, Null}
import org.joda.time.Interval
import primevc.mvc.controller.SubRouter
import primevc.core.traits.{VOCompanion, ValueObject}
import primevc.utils.ConvertTo
import primevc.types.{Field, Type}

trait RequestMap
{
  lazy val requestMap = scala.collection.JavaConversions.asScalaMap(request.raw.getParameterMap().asInstanceOf[java.util.Map[String, Array[String]]])
  lazy val requestMapReverseSorted: collection.immutable.SortedMap[String, Array[String]] = collection.immutable.SortedMap(requestMap.toList: _*)(Ordering.String.reverse)
}

trait XHTML
{
  def errorHTML(title:String): Elem = if (title == null || title == "") null else <div id="errorMsg"><h3>{title}</h3></div>

  var title = "Untitled page"
  var javaScript: String = null
  var javaScriptIncludes: List[String] = List(
    "/js/jquery-1.4.2.min.js",
    "/js/jquery-ui-1.8.4.custom.min.js",
    "/js/jquery.ui.datepicker-nl.js",
    "/js/uni-form.jquery.js",
    "/js/jquery.tablesorter.min.js",
    "/js/formutil.js"
  );


  response.contentType("application/xhtml+xml");

  def html(body:Elem*) =
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en-us" lang="en-us">
  <head>
    <title>{title}</title>
    <link href="/css/main.css" media="all" rel="stylesheet"/>
    <link href="/css/uni-form.css" media="all" rel="stylesheet"/>
    <link href="/css/dark.uni-form.css" media="all" rel="stylesheet"/>
  	<link href="/css/tablesorter.css" type="text/css" media="print, projection, screen" rel="stylesheet"/>
		<link href="/css/redmond/jquery-ui-1.8.4.custom.css" rel="stylesheet"/>
  </head>
  <body>
	  { errorHTML("Oops") }
    { body }
    { javaScriptIncludes.map(js => <script type="text/javascript" src={ js }></script>) }
    {if (javaScript != null)
      <script type="text/javascript">{ scala.xml.Unparsed(javaScript) }</script> }
  </body>
</html>
}

class CRUDRouter(parent:RequestRouter) extends SubRouter(parent) with RequestMap with XHTML with ListTable
{
  protected var prefixedVO: collection.mutable.Map[String, ValueObject] = collection.mutable.Map()

  lazy val validationErrors: List[(String,String)] = {
    println("-- validationErrors")
    var errors: List[(String,String)] = Nil;
    for ((pfx, vo) <- prefixedVO) {
      errors :::= vo.validationErrors_?.map({ e => (pfx + e._1.name, pfx + e._2) })
    }
    errors
  }

  def rowCountFor(inputID:String): Int = {
    for (k <- requestMapReverseSorted.keysIterator) {
      if (k.length > inputID.length + 2 && k.startsWith(inputID))
        return k.substring(inputID.length + 1, k.indexOf(']', inputID.length + 1)).toInt
    }
    0
  }
  def value(name: String) = requestMap.get(name).map(_(0)).orNull

  override def errorHTML(title:String): Elem = if (validationErrors.isEmpty) null
    else Form.errorMsg(title, validationErrors.iterator.map(e => Form.ErrorMessage(inputID = e._1, text = e._2)))

  def fillVO[A <: ValueObject](a: VOCompanion[A]) = {
    val vo = RequestVO(requestMapReverseSorted, Set[String](), a)
    prefixedVO("") = vo
	  vo
  }

  def fillVO[A <: ValueObject](skipKeys:Set[String], a: (String, VOCompanion[A])) = {
    val vo = RequestVO(requestMapReverseSorted, skipKeys, a)
    prefixedVO(a._1) = vo
	  vo
  }

  def fillVO[A <: ValueObject, B <: ValueObject](skipKeys: Set[String], a: (String, VOCompanion[A]), b: (String, VOCompanion[B])) = {
    val vo = RequestVO(requestMapReverseSorted, skipKeys, a, b)
    prefixedVO(a._1) = vo._1
    prefixedVO(b._1) = vo._2
	  vo
  }
}

trait Snippet
{
  val label = (s:String) => msg.get(s).getOrElse(s)
}

trait FormSnippet extends Snippet with FormBody {
  import Form._

  val parent        : CRUDRouter
  def formBody      : Elem
  val inputNamePrefix = ""

//  def value(inputID:String) = parent.value(inputID)
//  def rowCountFor(inputID:String): Int = parent.rowCountFor(inputID:String)

  implicit def list2Iter[T](l:List[T]) = l.iterator

  def field(field_ : Field, inputFn: (String, String) => Elem, hint: NodeSeq = null): Elem = {
    val name = inputNamePrefix + field_.name.name
    println(name, "required?", field_.required)
    Form.field(label(name), inputFn(name, value(name)),
      required = field_.required,
      error = parent.validationErrors.exists(_._1 == name),
      hint = hint)
  }

  def defaultFields(fields:Iterator[Field]): NodeSeq = fields map {
    f:Field => defaultField(inputNamePrefix, f, parent.validationErrors)
  } toSeq
}

trait VOFormSnippet[V <: ValueObject] extends FormSnippet {
  val VO : VOCompanion[V];
  var vo : V = _;

  def defaultFields : NodeSeq = defaultFields(VO.fields.iterator)

  def value(inputID:String): String = {
    val v = parent.value(inputID)
    if (v == null && vo != null) {
      println("Find value in VO: "+inputID)
      value(vo, VO, inputID.substring(inputNamePrefix.length))
    }
    else v
  }

  def value(vo:ValueObject, voc:VOCompanion[_], key:String): String =
  {
    import Type._

    val dot = key.indexOf('.')
    val bracketOpen = key.indexOf('[')

    if (dot - bracketOpen == 0) { // No . and no [
      voc.getValue(vo, key) match { case null => ""; case v => ConvertTo.string(v) }
    }
    else
    {
      val fieldName = key.substring(0, if (bracketOpen == -1 || dot > 0 && dot < bracketOpen) dot else bracketOpen)
      println("get field: " + fieldName)
      val fieldIndex = voc.field(fieldName)
      require(fieldIndex != -1, "Field not found: " + fieldName)
      val field = voc.field(fieldIndex)
      field.valueType match {
        case Tdef(subvoc, _) =>
          require(dot >= 0, "Not a property: "+key);
          value(voc.getValue(vo, fieldName).asInstanceOf[ValueObject], subvoc, key.substring(dot + 1))

        case Tarray(innerType,_,_) =>
          require(bracketOpen > 0 && bracketOpen < key.length - 2, "Not an array setter:" + key)
          val bracketClose = key.indexOf(']', bracketOpen + 1)
          require(bracketClose != -1);

          val index = key.substring(bracketOpen+1, bracketClose).toInt
          val arr:Array[_] = voc.getValue(vo, fieldName).asInstanceOf[Array[_]]
          if (index < arr.length) innerType match {
            case Tdef(subvoc, _) =>
              require(dot == bracketClose+1)
              println("get vo-array["+index+"] value: "+key)
              value(arr(index).asInstanceOf[ValueObject], subvoc, key.substring(dot+1))
            case _ => ConvertTo.string(arr(index))
          }
          else ""

        case Tinterval =>
            val interval:Interval = voc.getValue(vo, fieldIndex).asInstanceOf[Interval]
            if (interval == null) ""
            else if (key.endsWith("end")) ConvertTo.string(interval.getEnd)
            else ConvertTo.string(interval.getStart)
      }
    }
  }

  def rowCountFor(inputID:String) = if (vo == null) parent.rowCountFor(inputID) else VO.getValue(vo, inputID.substring(inputNamePrefix.length)) match {
    case v:Array[_] => v.length
//    case _ => parent.rowCountFor(inputID)
  }
}

trait NoArrayInputs {
  this : VOFormSnippet[_] =>
  override def rowCountFor(inputID:String) = 0
}
