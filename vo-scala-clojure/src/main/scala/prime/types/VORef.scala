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
  def     ? (implicit voGetter : V#IDType => Option[V]) : Option[V];
  def   get : V;
  val   _id : V#IDType;
}

object VORef {
  import Conversion._
  import ClojureProtocolVars._

  def apply [V <: ValueObject with ID](vo : V)                              : VORef[V] = if (vo._id != vo.voCompanion.empty.asInstanceOf[V]._id) new VORefImpl(vo._id, vo) else null;
  def apply [V <: ValueObject with ID](id : V#IDType)(implicit V : IVOC[V]) : VORef[V] = if (id != V.empty._id) new VORefImpl(id, V.empty) else null;

  def apply [V <: ValueObject with ID, IDType <: V#IDType](V  : IVOC[V], id : IDType) : VORef[V] = if (id != V.empty._id) new VORefImpl(id, V.empty) else null;
  def apply [V <: ValueObject with ID, IDType <: V#IDType](value : Any)(implicit V : IVOC[V], IDType : Any => IDType) : VORef[V] = unpack(value) match {
    case r:VORefImpl[_] =>
      require(V.manifest.VOType.erasure.isInstance(r._cached), V.empty.getClass+" incompatible with "+r._cached.getClass);
      r.asInstanceOf[VORef[V]]
    case r:  Ref[_] => new VORefImpl(IDType(r.ref), V.valueOf(r.vo_!));
    case V(value)   => vo2ref(value)
    case None       => throw NoInputException;
    case value      =>
      value match {
        case source.ValueSource(src) => try { return VORef(V(src)); } catch { case FailureException => }
        case _ =>
      }
      try VORef(IDType(value))(V) catch {
        case FailureException => vo_ref.invoke(value, V).asInstanceOf[VORef[V]];
      }
  }

  def valueOf[V <: ValueObject with ID](any : Any)(empty : V) = {
    val voCompanion = empty.voCompanion.asInstanceOf[ValueObjectCompanion[V]];
    val voManifest  = empty.voManifest .asInstanceOf[ValueObjectManifest [V] with IDField];
    VORef[V, V#IDType](any)(voCompanion, (voManifest._id.valueType.convert _).asInstanceOf[Any => V#IDType]);
  }
}

protected[prime] object VORefImpl {
  RT.load("prime/vo");
  val derefVar = RT.`var`("prime.vo", "*deref-map*");
}

protected[prime] final class VORefImpl[V <: ValueObject with ID](val _id:V#IDType, var _cached : V) extends VORef[V]
 with IDeref
 with IBlockingDeref
 with IPending
 with ClojureFn
{
  assert(_cached != null, "VORefImpl requires _cached to be at least the empty ValueObject.");
  if (isDefined) require(_id != null, toString + "._id should be non-null when _cached is set.");
  
  @inline def isEmpty   = _cached.voCompanion.empty == _cached;
  @inline def isDefined = _cached.voCompanion.empty != _cached;

  def apply(vo : V) {
    if (vo != null && vo._id == _id) synchronized { _cached = vo; }
    else throw new IllegalArgumentException(toString + (if (vo == null) " cannot cache a null VO." else "._id does not match the given VO._id: "+ vo));
  }

  
  def   ? (implicit voGetter : V#IDType => Option[V]) = if (isDefined) Some(_cached) else voGetter(_id) match { case opt @ Some(vo) => apply(vo); opt; case None => None }
  def get = if (isDefined) _cached else throw new NoSuchElementException(toString + ".get");

  override def toString = "VORef["+ _cached.voManifest.VOType.erasure.getSimpleName +"]" + (if (isDefined) "(_id = "+ _id +")" else "(_id = "+ _id +", _cached = "+ _cached +")");

  // Clojure's `ref` behavior
  private def derefFn = {
    val derefFn = VORefImpl.derefVar.invoke(_cached.voManifest.VOType.erasure).asInstanceOf[clojure.lang.IFn];
    require(derefFn != null, "No VO deref-function found in "+ VORefImpl.derefVar +" for: " + _cached.voManifest.VOType + " (voManifest: " + _cached.voManifest + ")");
    derefFn
  }

  def deref = if (isDefined) _cached else {
    val vo = derefFn.invoke(_id).asInstanceOf[V];
    apply(vo);
    vo;
  }

  def deref(timeoutInMs: Long, timeoutValue: Any) = {
    val vo = derefFn.invoke(_id, timeoutInMs, timeoutValue).asInstanceOf[V];
    if (vo != timeoutValue) apply(vo);
    vo;
  }

  def isRealized = isDefined;

  // Clojure's `promise` behaviour
  override def invoke  (vo : AnyRef)   = vo match {
    case vo if _cached.voManifest.VOType.erasure.isInstance(vo) =>
      apply(vo.asInstanceOf[V]);
      this;
    case _ => null
  }
  final    def applyTo (arglist: ISeq) = RT.boundedLength(arglist, 20) match {
    case 1 => invoke(arglist.first);
    case n => throwArity(n);
  }

  override def equals (other : Any) = (other.asInstanceOf[AnyRef] eq this) || (other match {
    case o : VORefImpl[_] => o._id == this._id && (_cached.voManifest.VOType.erasure.isInstance(o._cached)   /* Needed?: *//* || o._cached.voManifest.VOType.erasure.isInstance(_cached)*/);
    case o : VORef[_]     => try { o._id == this._id && (_cached.voManifest.VOType.erasure.isInstance(o.get) /* Needed?: *//* ||     o.get.voManifest.VOType.erasure.isInstance(_cached)*/) } catch { case _ => false; }
    case _ => false;
  })
  override def hashCode = _id.hashCode ^ _cached.hashCode;
}
