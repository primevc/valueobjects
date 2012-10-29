package prime.vo.source;
 import prime.vo.source._
 import prime.vo.mutable.{ValueObject => MutableValueObject}
 import prime.types.Field;

trait MutableVOValueSourceLike extends ValueSource with NoPrimitives {
  val vo : MutableValueObject;
  override def typeID (baseTypeID : Int) : Int = vo.voCompanion.TypeID;

  def field    (name: String, idx: Int) : Field   = {
    val voc = vo.voCompanion;
    try voc.fieldNamed(name) catch { case _ => 
    try voc.field(if (idx < 32) idx else if (idx > 0x010000) idx & 0xFF else -1) catch { case _ => 
    null }}
  }
  def contains (name: String, idx: Int)                : Boolean = field(name,idx) match { case f:Field => vo.fieldIsSet_?(f.name);                                             case null => false; }
  def anyAt    (name: String, idx: Int, notFound: Any) : Any     = field(name,idx) match { case f:Field if vo.fieldIsSet_?(f.name) => vo.voCompanion.getValue(vo, f.name.name); case _ => notFound; }
}

class MutableVOValueSource(val vo : MutableValueObject) extends MutableVOValueSourceLike

object MutableVOValueSource {
  implicit def apply(vo : MutableValueObject) : ValueSource = new MutableVOValueSource(vo);

  prime.vo.source.ValueSource;

  import clojure.lang.RT.{ `var` => v }
  v("clojure.core", "eval").invoke(v("clojure.core","read-string").invoke("""
    (extend-type prime.vo.mutable.ValueObject
      prime.vo.source/ValueSourceable
    (as-source
      ([vo]      (prime.vo.source.MutableVOValueSource/apply vo))
      ([vo kind] (prime.vo.source.MutableVOValueSource/apply vo))))"""));
}
