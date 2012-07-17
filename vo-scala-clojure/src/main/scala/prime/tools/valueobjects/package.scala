package prime.types;
 import prime.vo.mutable._

package object valueobjects
{
  implicit def vo2ref[T <: ValueObjectWithID](vo:T)
                                             (implicit refID_helper:IDAccessor[T]): Ref[T] = new Ref(refID_helper.idValue(vo), vo)
  implicit def ref2vo[T <: ValueObjectWithID](ref:Ref[T])
                                             (implicit refsProxy:VOProxy[T]) : T = ref.vo.get
}
