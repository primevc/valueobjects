package primevc.mvc.crud

import xml.Elem
import primevc.types._

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 20-sep-2010
 * Time: 12:13:29
 * To change this template use File | Settings | File Templates.
 */

trait FormBody
{
  import Form._

  // Abstracts
  def label : String => String
  def value(inputID : String): String
  def rowCountFor(inputID : String): Int

  def label   (field:Field)     : String = label(field.name.name)
  def label   (fieldPath:Symbol): String = label(fieldPath.name)


  def valueFor(fieldPath:String, orElse:String = null): String = {
    try {
      val v = value(fieldPath)
      if (v != null) v else orElse
    } catch {
      case e:java.util.NoSuchElementException => orElse
    }
  }

  def options (fieldPath:String): Option[Iterator[SelectOption]]

  // Field generators
  def emptyThenOptions(name:String) = options(name) match {
    case Some(o) => Iterator(EmptyOption) ++ o
    case _ => Iterator(EmptyOption)
  }

  val EmptyOption = SelectOption("", label("-select-"))

  protected def enumOptions(name:String, t:Enumeration): (Int, Set[SelectOption]) = {
    var numOpts = 0;
    val options = t.values.filter(_.toString.length != 0) map { v:t.Value => numOpts += 1; SelectOption(v.toString, label(name+"."+v.toString)) }
    (numOpts, options)
  }
  def refField(name:String) = field(label(name), select(emptyThenOptions(name))(name))

  def defaultField(voField:Field, errors:List[(String, String)]): Elem =
      defaultField(voField.name.name, voField.valueType, required = voField.required, errors = errors);

  def defaultField(inputNamePrefix:String, voField:Field, errors:List[(String, String)]): Elem =
      defaultField(label(inputNamePrefix + voField.name.name), voField.valueType, required = voField.required, errors = errors)

  def defaultField(name:String, valueType:Type.T, required:Boolean, errors:List[(String, String)]): Elem = {
    import Type._
    val myLabel = this.label(name)
    def field(label:String, xml:Elem) = Form.field(label, xml, error = errors.exists(_._1 == name))

    valueType match
    {
      case  Tdef		  (vo,ref) =>
        if (ref) refField(name)
        else {
          multifield(myLabel, name, vo.fields map {
              f => defaultField(name + ".", f, errors)
            } toSeq, false, false, null, false)
        }

      case  Tenum     (t) =>
        //TODO: Add textfield for 'other' enum values
        val (numOpts, options) = enumOptions(name, t)
        if (numOpts > 6)
          field(myLabel, select(Iterator(EmptyOption) ++ options.iterator)(name, valueFor(name)))
        else
          radiofield(myLabel, name, options.iterator,
            error = errors.exists(_._1 == name),
            columns = numOpts > 3,
            selectedValue = valueFor(name))

      case  Tarray		(innerType,	_, _)  =>
        val rowCount = rowCountFor(name)
        innerType match {
        case Tdef (vo,ref) =>
          if (ref)
            field(myLabel, arrayTable(name, customInputs = Seq((name, select(emptyThenOptions(name))(name))),
              rowCount = rowCount))
          else
            field(myLabel, arrayTable(name, vo.fields, rowCount = rowCount))

        case _ => field(myLabel,
            arrayTable(name, List(Field(Symbol(""), innerType, false)), // customInputs = List((name, defaultInput(innerType, name + "[0]"))),
              rowCount = rowCount))
      }

      case  Tbool			(default) => checkfield(myLabel, name, default)
      case  Tinteger	(_,_,_) => field(myLabel, smallTextInput()(name, valueFor(name)))
      case  Tdecimal	(_,_,_) => field(myLabel, smallTextInput()(name, valueFor(name)))
      case  Tdate     => <p>todo: {valueType.toString}</p>
      case  Tdatetime => <p>todo: {valueType.toString}</p>
      case  Tinterval =>
        val start = name+".start"
        val end   = name+".end"

        multifield(myLabel, name,
          Iterator((label(start), dateInput(start, valueFor(start))),
                   (label(end),   dateInput(end,   valueFor(end)))),
          columnLayout = true)

      case  Tcolor    => <p>todo: {valueType.toString}</p>
      case  Tbitmap   => <p>todo: {valueType.toString}</p>
      case  Tstring   => field(myLabel, textInput()(name, valueFor(name)))
      case  Turi      => field(myLabel, textInput()(name, valueFor(name, "http://")))
      case  Temail    => field(myLabel, textInput()(name, valueFor(name)))
      case  TuniqueID => <div class="ctrlHolder"><label>{myLabel}</label><p>{valueFor(name, "?")}</p></div>
    }
  }

  def arrayTable(name:String, defaultInputFields:Iterable[Field] = List(), customInputs:Seq[(String,Elem)] = Seq(), rowCount:Int = 0): Elem =
  <table id={name} class="arraytable" cellpadding="0">
    <thead>
      <tr><td>#</td>
        { defaultInputFields.iterator.map(_.name.name) ++
                customInputs.iterator.map(_._1) map { f => <th>{ label(f) }</th> }}
      </tr>
    </thead>
    <tbody>
    { (0 to rowCount) map { row =>
      <tr><td>{ row + 1 }</td>
        {(defaultInputFields.iterator map { f:Field => defaultInput(f.valueType, name + "["+ row +"]." + f.name.name) }) ++
                customInputs.iterator.map(_._2) map { i => <td>{i}</td> }}
      </tr> }
    }
    </tbody>
  </table>

  def defaultInput(inputType:Type.T, name:String): Elem = {
    import Type._
    inputType match {
      case  Tdef		  (vo,ref) => select(emptyThenOptions(name))(name)
      case  Tenum     (t) => select(Iterator(EmptyOption) ++ enumOptions(name, t)._2.iterator)(name)
      case  Tbool			(default) => checkbox(name, default)
      case  Tinteger	(_,_,_) => smallTextInput()(name, valueFor(name))
      case  Tdecimal	(_,_,_) => smallTextInput()(name, valueFor(name))
      case  Tdate     => <p>todo: {inputType.toString}</p>
      case  Tdatetime => <p>todo: {inputType.toString}</p>
      case  Tinterval =>
        val start = name+".start"
        val end   = name+".end"
        <div>{ dateInput(start, valueFor(start)) }{ dateInput(end, valueFor(end)) }</div>

      case  Tcolor    => <p>todo: {inputType.toString}</p>
      case  Tbitmap   => <p>todo: {inputType.toString}</p>
      case  Tstring   => textInput()(name, valueFor(name))
      case  Turi      => textInput()(name, valueFor(name, "http://"))
      case  Temail    => textInput()(name, valueFor(name))
      case  TuniqueID => <i>{ valueFor(name, "?") }</i>

      case _ => <b>no default input for: {inputType.getClass.getSimpleName} {name}</b>
    }
  }
}
