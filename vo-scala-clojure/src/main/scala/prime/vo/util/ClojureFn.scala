package prime.vo.util;
 import java.util.concurrent.Callable;
 import clojure.lang.{ArityException, IFn};

trait ClojureFn extends IFn
{
  this: Callable[_] =>

  protected final def throwArity(n:Int) : AnyRef = throw new ArityException(n, this.getClass.getSimpleName);
  
  // Nonsense invocations:
  def invoke(): AnyRef = throwArity(0)
  def invoke(arg1: AnyRef) : AnyRef = throwArity(1)
  def invoke(arg1: AnyRef, arg2: AnyRef): AnyRef = throwArity(2)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef) : AnyRef = throwArity(3)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef): AnyRef = throwArity(4)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef) : AnyRef = throwArity(5)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef): AnyRef = throwArity(6)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef) : AnyRef = throwArity(7)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef): AnyRef = throwArity(8)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef) : AnyRef = throwArity(9)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef) : AnyRef = throwArity(10)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef) : AnyRef = throwArity(11)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef) : AnyRef = throwArity(12)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef) : AnyRef = throwArity(13)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef) : AnyRef = throwArity(14)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef, arg15: AnyRef) : AnyRef = throwArity(15)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef, arg15: AnyRef, arg16: AnyRef) : AnyRef = throwArity(16)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef, arg15: AnyRef, arg16: AnyRef, arg17: AnyRef) : AnyRef = throwArity(17)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef, arg15: AnyRef, arg16: AnyRef, arg17: AnyRef, arg18: AnyRef) : AnyRef = throwArity(18)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef, arg15: AnyRef, arg16: AnyRef, arg17: AnyRef, arg18: AnyRef, arg19: AnyRef) : AnyRef = throwArity(19)
  def invoke(arg1: AnyRef, arg2: AnyRef, arg3: AnyRef, arg4: AnyRef, arg5: AnyRef, arg6: AnyRef, arg7: AnyRef, arg8: AnyRef, arg9: AnyRef, arg10: AnyRef, arg11: AnyRef, arg12: AnyRef, arg13: AnyRef, arg14: AnyRef, arg15: AnyRef, arg16: AnyRef, arg17: AnyRef, arg18: AnyRef, arg19: AnyRef, arg20: AnyRef) : AnyRef = throwArity(20)
  def invoke(arg1: Any,    arg2: Any,    arg3: Any,    arg4: Any,    arg5: Any,    arg6: Any,    arg7: Any,    arg8: Any,    arg9: Any,    arg10: Any,    arg11: Any,    arg12: Any,    arg13: Any,    arg14: Any,    arg15: Any,    arg16: Any,    arg17: Any,    arg18: Any,    arg19: Any,    arg20: Any,    args: AnyRef*): AnyRef = throwArity(20 + args.length)

  final def run () { invoke() }
  final def call() = invoke()
}
