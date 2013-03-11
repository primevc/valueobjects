package prime.vo.source

trait IntAnyArrayValueSource extends ValueSource with NoPrimitives {
  val ids    : Array[Int];
  val values : Array[Any];
  val typeID : Int;

  @inline final def find (idWithIndex : Int) : Int = {
    val id    =  idWithIndex >>> 8;
    val mixin = (idWithIndex >>> 8) & 0xFFFF00;

    var i = 0;
    while (i < ids.length)
    {
      val item  = ids(i);
      if (item == id) return i; // Found!

      /*
        We know:
        - ids are grouped togheter by mixin
        - within a group, ids are sorted by field id: [0x100, 0x101, 0x102, ...]
      */
      if ((item & 0xFFFF00) == mixin && (item > id /* Current item is already past the ID we're looking for */ || 
          (i < (ids.length-2) && (ids(i + 1) & 0xFFFF00) != mixin) /* Next ID is no longer part of this mixin. */))
        return -1; // (2) No more values for this mixin...

      i += 1;
    }
    -1;
  }

  override def typeID(baseTypeID:Int) = typeID;
  @inline final def contains (ignored: String, idx : Int)                : Boolean = find(idx) != -1
  @inline final def anyAt    (ignored: String, idx : Int, notFound: Any) : Any     = { val i = find(idx); if (i != -1) values(i) else notFound }

  override def toString = getClass.getSimpleName +"("+ ids +", "+ values +")";
}
