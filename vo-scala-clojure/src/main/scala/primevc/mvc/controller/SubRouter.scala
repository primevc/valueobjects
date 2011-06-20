package primevc.mvc.controller
 import ru.circumflex.web._

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: Sep 3, 2010
 * Time: 4:10:30 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class SubRouter(val parent:RequestRouter)
  extends RequestRouter
{
  prefix = parent.prefix + "/" + this.getClass.getSimpleName.toLowerCase
  get("") = redirect(prefix+"/")
}
