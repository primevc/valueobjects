package primevc.mvc.crud
import xml.Elem
import ru.circumflex.core._
import primevc.core.traits.{VOProxy, ValueObjectWithID, VOAccessor, ValueObject}
import primevc.utils.ConvertTo
import primevc.mvc.controller.SubRouter
import primevc.mvc.mongodb.MongoProxy

/**
 * Created by IntelliJ IDEA.
 * User: Danny
 * Date: 28-okt-2010
 * Time: 12:31:10
 * To change this template use File | Settings | File Templates.
 */
trait ListTable
{
  this : XHTML with SubRouter =>

  def thead(fields:Iterable[String]) =
  <thead>{ fields map { f => <th>{ msg.get(f).getOrElse(f) }</th>
  }}</thead>

  def table[V <: ValueObject](proxy : VOAccessor[V])
                             (items : Iterator[V], fields : Iterable[String]):Elem =
  {
    <table class="tablesorter">
    { thead(fields) }
    <tbody>
    {
      items map
      { vo:V =>
        <tr>
        {
          fields map { f =>
            <td>{ ConvertTo.string(proxy.getValue(vo, f)) }</td> }
        }
        </tr>
      }
    }
    </tbody>
    </table>
  }

  def table[V <: ValueObjectWithID](proxy : VOProxy[V])
                                   (items : Iterator[V], fields : Iterable[String]):Elem =
  {
    <table class="tablesorter">
    { thead(fields) }
    <tbody>
    {
      items map
      { vo:V =>
        <tr>
        {
          fields map { f =>
            <td>{ /*ConvertTo.string(proxy.getValue(vo, f))*/ }</td> }
        }
          <td><a href={ prefix + ConvertTo.string(proxy.idValue(vo)) + "/edit" }>Edit</a></td>
        </tr>
      }
    }
    </tbody>
    </table>
  }

  def mTable[V <: ValueObjectWithID](proxy : MongoProxy[V])
      (items : Iterator[V] = proxy.find(), fields : Iterable[String]): Elem =
      table(proxy)(items,fields)

  javaScript += "$(document).ready(function() { $('table').tablesorter(); });"
  javaScriptIncludes ::= "/js/jquery.tablesorter.min.js"
}
