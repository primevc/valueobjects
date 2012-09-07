package prime.types;
 import prime.vo._
 import prime.types._
 import prime.vo.util.ClojureFn
 import clojure.lang.{IDeref, IBlockingDeref, IPending, IFn, ISeq, RT};
 import prime.vo.{ValueObjectCompanion => IVOC};


trait VORef[V <: ValueObject with ID] {
  def isEmpty   : Boolean;
  def isDefined : Boolean;

  def apply (vo : V);
  def     ? (implicit voProxy : V#IDType => Option[V]) : Option[V];
  def   get : V;
  val   _id : V#IDType;
}

object VORef {
  import Conversion._
  import ClojureProtocolVars._

  def apply [V <: ValueObject with ID, IDType <: V#IDType](value : Any)(implicit V : IVOC[V], IDType : Any => IDType) : VORef[V] = unpack(value) match {
    case r:VORefImpl[V] => assert(r._cached.getClass eq V.empty.getClass); r
    case r:  Ref[_] => new VORefImpl(r.ref, V.valueOf(r.vo_!));
    case V(value)   => vo2ref(value)
    case None       => throw NoInputException;
    case value      => try VORef(V, IDType(value)) catch { case FailureException => vo_ref.invoke(value, V).asInstanceOf[VORef[V]]; }
  }

  def apply [V <: ValueObject with ID, IDType <: V#IDType](V : IVOC[V], id : IDType) : VORef[V] = if (id != V.empty._id) new VORefImpl(id, V.empty) else null;
}

protected[prime] object VORefImpl {
  RT.loadResourceScript("prime/vo.clj");
  val proxyVar = RT.`var`("prime.vo", "*proxy-map*");
}

protected[prime] final class VORefImpl[V <: ValueObject with ID](val _id:V#IDType, var _cached : V) extends VORef[V]
 with IDeref
 with IBlockingDeref
 with IPending
 with ClojureFn
{
  if (isDefined) require(_id != null, toString + "._id should be non-null when _cached is set.");
  
  @inline def isEmpty   = _cached.voCompanion.empty == _cached;
  @inline def isDefined = _cached.voCompanion.empty != _cached;

  def apply(vo : V) = if (vo._id == _id) synchronized { _cached = vo; } else throw new IllegalArgumentException(toString + "._id does not match the given VO._id: "+ vo);

  
  def   ? (implicit voProxy : V#IDType => Option[V]) = if (isDefined) Some(_cached) else voProxy(_id) match { case opt @ Some(vo) => apply(vo); opt; case None => None }
  def get = if (isDefined) _cached else throw new NoSuchElementException(toString + ".get");

  override def toString = "VORef["+ _cached.voManifest.VOType.erasure.getSimpleName +"]" + (if (isDefined) "(_id = "+ _id +")" else "(_id = "+ _id +", _cached = "+ _cached +")");

  // Clojure's `ref` behavior
  private def proxy = {
    val proxy = VORefImpl.proxyVar.invoke(_cached.voManifest.VOType.erasure).asInstanceOf[clojure.lang.IFn];
    require(proxy != null, "No VOProxy found in "+ VORefImpl.proxyVar +" for: " + _cached.voManifest.VOType + " (voManifest: " + _cached.voManifest + ")");
    proxy
  }

  def deref = if (isDefined) _cached else {
    val vo = proxy.invoke(_id).asInstanceOf[V];
    apply(vo);
    vo;
  }

  def deref(timeoutInMs: Long, timeoutValue: Any) = {
    val vo = proxy.invoke(_id, timeoutInMs, timeoutValue).asInstanceOf[V];
    if (vo != timeoutValue) apply(vo);
    vo;
  }

  def isRealized = isDefined;

  // Clojure's `promise` behaviour
  override def invoke  (vo : AnyRef)   = if (isEmpty){ apply(vo.asInstanceOf[V]); this } else null;
  final    def applyTo (arglist: ISeq) = RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case n => throwArity(n);
  }

  override def equals (other : Any) = (other.asInstanceOf[AnyRef] eq this) || (other match {
    case o : VORefImpl[V] => o._id == this._id;
    case o : VORef[V]     => try { o.get._id == this.get._id } catch { case _ => false; }
    case _ => false;
  })
}
