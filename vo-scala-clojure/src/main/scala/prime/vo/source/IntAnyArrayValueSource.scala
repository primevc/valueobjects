package prime.vo.source

trait IntAnyArrayValueSource extends ValueSource with NoPrimitives {
  val ids    : Array[Int];
  val values : Array[Any];
  val typeID : Int;

//  var lastID : Int = -1;
//  var lastI_ID : Int = -1;

  @inline final def find (idWithIndex : Int) : Int = {
    val id    =  idWithIndex >>> 8;
    val mixin = (idWithIndex >>> 8) & 0xFFFF00;
//    val lastID = lastI_ID & 0xFFFFFF;

    /*
      We know:
      - ids are grouped togheter by mixin
      - within a group, ids are sorted by field id: [0x100, 0x101, 0x102, ...]

      This allows us to
      - (1) skip one item ahead in the search, provided the last found item was part of the same mixin as the one we're currently looking for.
      - (2) stop looking when mixin ID changes while looking for the requested item.
    */
    var i = 0;// if (lastID <= id && mixin == (lastID & 0xFFFF00)) /*(1)*/ (lastI_ID >>> 24); else 0;
    while (i < ids.length)
    {
      val item = ids(i);
      if (item == id) {
        //lastI_ID = (i << 24) | item;
        return i; // Found!
      }

      if ((item & 0xFFFF00) == mixin && (item > id || (i < (ids.length-2) && (ids(i + 1) & 0xFFFF00) != mixin)))
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

/*
x86_64 Mac OS X 10.8.2 4 cpu(s)
Java HotSpot(TM) 64-Bit Server VM 20.12-b01-434
Runtime arguments: -XX:+TieredCompilation -Dclojure.compile.path=/Users/blue/Development/Online Touch/backend/storm/target/classes -Dstorm-converter.version=1.0.0-SNAPSHOT -Dclojure.debug=false

== With lastI_ID:

Evaluation count : 840 in 60 samples of 14 calls.
      Execution time sample mean : 104,059785 ms
             Execution time mean : 104,090370 ms
Execution time sample std-deviation : 23,111100 ms
    Execution time std-deviation : 23,645076 ms
   Execution time lower quantile : 44,587071 ms ( 2,5%)
   Execution time upper quantile : 142,176879 ms (97,5%)


== Without:

Evaluation count : 840 in 60 samples of 14 calls.
      Execution time sample mean : 89,166730 ms
             Execution time mean : 89,108850 ms
Execution time sample std-deviation : 23,751169 ms
    Execution time std-deviation : 23,924308 ms
   Execution time lower quantile : 43,262214 ms ( 2,5%)
   Execution time upper quantile : 126,421586 ms (97,5%)
*/
