package primevc.mvc.crud

import xml.{Text, Attribute, Elem, NodeSeq, Null}
import ru.circumflex.core._
import ru.circumflex.web._
import org.joda.time.Interval
import primevc.types._
import primevc.core.traits._
import collection.immutable.SortedMap

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: 20-sep-2010
 * Time: 12:11:35
 * To change this template use File | Settings | File Templates.
 */
class Form(router:RequestRouter, body: => Elem)
{
  val token = router.prefix +"@"+ request.sessionId
  val enctype = "application/x-www-form-urlencoded"
  var css:String = "uniForm"

  def html() =
  <form method="POST" class={css} enctype={enctype}><input type="hidden" name="_t" value={token}/>
    { body }
    <div class="buttonHolder">
      <a href={router.prefix} class="secondaryAction">← Annuleren</a>
      <button type="submit" class="primaryAction">Opslaan</button>
    </div>
  </form>

  var authorized_? = true;

  def POST[T]( onAuthenticated: => T ): T =
    if (param("_t") != token)
      router.error(400, msg("form.token_error"));
    else if (!authorized_?)
      router.error(401);
    else
      onAuthenticated;
}

object Form
{
  case class SelectOption(value:String, label:String)
  case class ErrorMessage(text:String, inputID:String)

//  def httpInput(name:String, value:String=null) =
//  <div class="money"><abbr title="Euro">€</abbr><input id={name} name={name} value={value} maxlength="16" type="text" class="textInput auto"/></div>

  def dateInput(name:String, value:String=null) =
  <input id={name} name={name} value={value} size="11" maxlength="11" type="text" class="textInput"/>

  def euroInput(name:String, value:String=null) =
  <div class="money"><abbr title="Euro">€</abbr><input id={name} name={name} value={value} maxlength="16" type="text" class="textInput auto"/></div>

  def smallTextInput(maxLength:Short=50)(name:String, value:String=null) =
  <input id={name} name={name} value={value} size="35" maxlength={maxLength.toString} type="text" class="textInput small"/>

  def textInput(maxLength:Short=50)(name:String, value:String=null) =
  <input id={name} name={name} value={value} size="35" maxlength={maxLength.toString} type="text" class="textInput"/>

  def textarea(name:String, value:String=null) =
  <textarea id={name} name={name} rows="25" cols="25">{value}</textarea>

  def select(options:Iterator[SelectOption])(name:String, selectedValue:String="") =
  <select id={name} name={name} class="selectInput auto">
  {
    options map { opt =>
      <option value={opt.value} selected={ if(opt.value == selectedValue) "'" else null }>{opt.label}</option>
    }
  }
  </select>

  def checkbox(name:String, selectedValue:String): Elem = checkbox(name, selectedValue != null && selectedValue.length != 0)
  def checkbox(name:String, selected:Boolean): Elem =
  <input id={name} name={name} value="1" type="checkbox" checked={if (!selected) null else "'"}/>

  def radioItem(name:String, value:String, label:String, selected:Boolean=false) =
  <li><label><input name={name} value={value} type="radio" checked={if (!selected) null else "'"}/> {label}</label></li>

  def radioItems(name:String, options:Iterator[SelectOption], selectedValue:String=null):NodeSeq =
  {
    options.toSeq map { opt =>
      radioItem(name, opt.value, opt.label, selected = (opt.value == selectedValue))
    }
  }

  def upload(name:String, ignoredValue:String): Elem = upload(name)
  def upload(name:String): Elem =
  <input id={name} name={name} size="35" type="file" class="fileUpload"/>

  def field(label:String, input:Elem, required:Boolean=false, error:Boolean=false, hint:NodeSeq=null) =
  {
    val inputNode:Elem =
      if (!error) input;
      else input % Attribute("class", Text(input.attribute("class").map(_.text + " error").getOrElse("error")), Null);

    <div id={(inputNode \\ "@id").headOption.map("f-" + _.text).orNull} class={if (!error) "ctrlHolder" else "ctrlHolder error"}>
      <label for={(inputNode \\ "@id").headOption.map(_.text).orNull}>{ if(required) <em>*</em> else null }{label}</label>
      {inputNode}
      { if (hint != null) <p class="formHint">{hint}</p> else null }
    </div>
  }

  def checkfield(label:String, name:String, selected:Boolean=false, required:Boolean=false, error:Boolean=false, hint:NodeSeq=null) =
  <div id={"f-"+name} class={if (!error) "ctrlHolder noLabel" else "ctrlHolder noLabel error"}>
    <ul>
      <li><label for={name}>{checkbox(name, selected)}{ if(required) <em>*</em> else null } {label}</label></li>
    </ul>
    <p class="formHint">{hint}</p>
  </div>

  def radiofield(label:String, name:String, options:Iterator[SelectOption], selectedValue:String=null,
                 required:Boolean=false, error:Boolean=false, hint:NodeSeq=null, columns:Boolean=false):Elem =
      multifield(label, name, radioItems(name, options, selectedValue), required, error, hint, columns)

  def multifield(label:String, idSuffix:String, ulBody:NodeSeq, required:Boolean, error:Boolean, hint:NodeSeq, columnLayout:Boolean):Elem =
  <div id={"f-" + idSuffix} class={if (!error) "ctrlHolder" else "ctrlHolder error"}>
    <p class="label">{ if(required) <em>*</em> else null }{label}</p>
    <ul class={if (!columnLayout) null else "alternate"}>{ulBody}</ul>
    { if (hint != null) <p class="formHint">{hint}</p> else null }
  </div>

  def multifield(label:String, idSuffix:String, inputs:Iterator[(String, Elem)],
                 required:Boolean=false, error:Boolean=false, hint:NodeSeq=null, columnLayout:Boolean=false):Elem =
    multifield(label, idSuffix,
      inputs.toSeq.map({ i => val (label, input) = i;
        <li><label>{label} {input}</label></li>
      }),
      required, error, hint, columnLayout
    );

  def fieldset(title:String, inlineLabels:Boolean = false)(body:NodeSeq*) =
  <fieldset class={if (inlineLabels) "inlineLabels" else null}>
    <h3>{title}</h3>
    {body}
  </fieldset>

  def errorMsg(title:String, errors:Iterator[ErrorMessage]) =
  <div id="errorMsg">
    <h3>{title}</h3>
    <ol>
    {
      errors map { e =>
        <li><a href={"#f-"+e.inputID}>{e.text}</a></li>
      }
    }
    </ol>
  </div>

  def okMsg(message:NodeSeq) =
  <div id="okMsg"><p>{message}</p></div>
}
