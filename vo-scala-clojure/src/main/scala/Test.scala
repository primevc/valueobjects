package vo.spread;
 import prime.vo._;
 import prime.vo.source._;
 import prime.types._;
 import prime.types.Conversion;

object time {
  def apply(block: => Unit) = {
    val start = java.lang.System.nanoTime()
    block
    val time = java.lang.System.nanoTime() - start
    println("time: " + time / 1e6 + " ms")
  }
}

//-- Box
trait Box extends ValueObject {
  val w : Int;

  def copy(w : Int = this.w): this.type;
}

final class BoxVO protected[vo](val voSource : ValueSource, val w : Int) extends ValueObject_1 with Box with source.Integers with LeafNode {
  type VOType = Box;
  final def voManifest  = Box.manifest;
  final def voCompanion = Box;

  //def srcDiff    = if (w == voSource.intAt(null, 0, this.w)) 0 else 1

  def intAt(name:String, idx:Int, notFound:Int) = if (name == "w" || Box.manifest.index(idx) == 0) w else notFound;

  def copy(w : Int = this.w) = new BoxVO(this.voSource, w).asInstanceOf[this.type];
  def copy(src: ValueSource, root: ValueSource) = new BoxVO(w = src.intAt("w",0,Box.empty.w), voSource = root).asInstanceOf[this.type]
}

object Box extends ValueObjectCompanion[Box] {
  val empty = new BoxVO(EmptyVO, w = -1)

  //BoxVO een package case class maken? Of zoiets?
  def apply(vo : Box): Box = vo;
  def apply(w : Int = empty.w): Box = empty.copy(w);

  object manifest extends {
    val ID = 12;

    val w = new ValueObjectField[Box](0, 'w, ValueTypes.Tinteger(), Box.empty.w    ) { def apply(vo: Box) = vo.w }
    val first = w;

    /*def apply(name : String) = name match {
      case "w"     => w;
    }*/

    //val lastFieldIndex = 0;
    //final def apply(idx : Int) = { index(idx); w }
  } with ValueObjectManifest_1[Box];
}

//
sealed abstract class FileType(val value:Int, override val toString:String, val toMime:String, val toExt:String) extends EnumValue;
object FileType extends Enum {
  type Value = FileType;

  object MP3 extends Value(73729, "MP3", "video/mp4", "mp4");
  object MP4 extends Value(73729, "MP4", "video/mp4", "mp4");

  val values = Set(MP3, MP4);

  def fromMime(str:String) = values.find(_.toMime == str).getOrElse(apply(str));
  def fromExt (str:String) = values.find(_.toExt  == str).getOrElse(apply(str));
}

sealed abstract class MimeType(val value:Int, override val toString:String, val toMime:String, val toExt:String) extends EnumValue;
object MimeType extends Enum {
  type Value = MimeType;

  object MP3 extends Value(73729, "MP3", "video/mp4", "mp4");
  object MP4 extends Value(73729, "MP4", "video/mp4", "mp4");

  val values = Set(MP3, MP4);

  def fromMime(str:String) = values.find(_.toMime == str).getOrElse(apply(str));
  def fromExt (str:String) = values.find(_.toExt  == str).getOrElse(apply(str));
}

//-- Spread
trait Spread extends ValueObject {
  val x : Int;
  def frame : Box;

  def copy(x : Int, frame : Box): this.type;
}


object Spread extends ValueObjectCompanion[Spread] {
  val empty = new SpreadVO(0, 0, EmptyVO, 123, Box.empty);

  def apply(x : Int = 123, frame : Box = empty.frame) = {
    assert(empty != null);
    empty.copy(x, frame)
  }

  object manifest extends {
    val ID = 3;

    val x = new ValueObjectField[Spread](0, 'x, ValueTypes.Tinteger(), Spread.empty.x) {
      //TODO: bench null vs Option
      def apply(vo: Spread) = vo.x
    } 
    val frame = new VOValueObjectField[Spread, Box](1, 'frame, Spread.empty.frame, false) {
      def apply(vo: Spread) = vo.frame
    }

    /*def apply(name : String) = name match {
      case "x"     => x;
      case "frame" => frame;
    }*/

    val fields = Array(x,frame)
    //val fieldIndexMask = 1 << 0 | 1 << 1;
    //val lastFieldIndex = 2;
  } with ValueObjectManifest_N[Spread];
}

final class SpreadVO protected[vo](
  voIndexSet : Int, srcDiff : Int, val voSource : ValueSource,

  val x : Int,
  private var frame0: Box = null
)
extends ValueObject_4(voIndexSet,srcDiff) with Spread with BranchNode
{
  type VOType = Spread;
  final def voManifest  = Spread.manifest;
  final def voCompanion = Spread;

// Not needed with voSource type-check in copy() ?
//  if (frame0 == null && !voSource.containsKey(1,"frame")) frame0 = Box.empty;

  final def frame = if (frame0 != null) frame0 else {
    synchronized  { if (frame0 == null) frame0 = Box.valueOf(voSource.anyAt("frame", 1, Spread.empty.frame)); }
    frame0;
  }

  def isRealized = frame0 != Spread.empty.frame;
  def realized   = {frame; self}

  def boolAt(name:String, idx:Int, notFound:Boolean) = (name,idx) match {
    case ("x",_) | (_, 0) => this.x == 1;
    case _ => notFound;
  }
  def intAt(name:String, idx:Int, notFound:Int) = (name,idx) match {
    case ("x",_) | (_, 0) => this.x;
    case _ => notFound;
  }
  def doubleAt(name:String, idx:Int, notFound:Double) = (name,idx) match {
    case ("x",_) | (_, 0) => this.x.toDouble;
    case _ => notFound;
  }

  @inline
  def differentAndEmptied(x : Int, frame : Box): Long = {
    val empty = Spread.empty;
    var diff = 0L;
    var emptied = 0;
    if (x != this.x) {
      diff |= (1 << 0); if (x     == Spread.empty.x    ) emptied |= (1 << 0);
    }
    if (frame != this.frame0 && frame != this.frame) {
      diff |= (1 << 1); if (frame == Spread.empty.frame) emptied |= (1 << 1);
    }

    (diff << 32) | emptied
  }
  
  protected def copy(x : Int, frame : Box, root : ValueSource) : this.type = {
    val frame_v = if (frame == null /*delete*/) Spread.empty.frame else frame;
    val diff_emptied = this.differentAndEmptied(x, frame_v)
    val diff = (diff_emptied >>> 32).toInt
    val emptied = (diff_emptied & 0xFFFFFFFF).toInt

    if (diff > 0)
    {
      val newSet = (this._voIndexSet | diff) ^ emptied;
      if (newSet > 0) new SpreadVO(newSet, diff, root, x, frame_v).asInstanceOf[this.type]
      else Spread.empty.asInstanceOf[this.type];
    }
    else this;
  }

  /** protected because vo.conj(voSource) or Companion(voSource) are friendlier interfaces. */
  override def copy(src : ValueSource, root : ValueSource) : this.type = this.copy(
    x     = try src.intAt("x",0, this.x) catch { case Conversion.NoInputException => Spread.empty.x },
    frame = Spread.manifest.frame(this, src, root, frame0),
  /*  frame = voManifest.frame(src, None) match {
      case v:Box => v;
      
      case ValueSource(vo) =>
        if (root != src) /*eager*/ Box(vo)
        else /*lazy*/ this.frame0;
      
      case Box(vo) => vo

      case _ => if (root != this.voSource) /*eager*/ this.frame else /*lazy*/ this.frame0;
    },*/

/* junk
    name    = String(src("name",1, this.name))
    frames  = ConvertTo.ArrayOf(Box, src.arrayOf(Box, 2, "frames", this.frames))
    ratings = ConvertTo.ArrayOfInt(src(3, "ratings", this.ratings)) // src krijgt nu te weinig info om te optimizen?
*/
    root  = root
  );

  def copy(x : Int = this.x, frame : Box = this.frame0) : this.type = copy(x, frame, root = this.voSource)
  
  def foreach(b: (ValueObjectField[Spread], Any) => Unit) {
    if ((_voIndexSet & (1 << 0)) > 0) b(Spread.manifest.x, this.x);
    if (frame != Spread.empty.frame) b(Spread.manifest.frame, this.frame0);
  }

/* // scraps
  def assoc(idx : Int, value : Any) : this.type = manifest.index(idx) match {
    case 0 => copy(x     = value)
    case 1 => copy(frame = value)
  }

  def assoc  (src          : ValueSource) : this.type = copy(src, root = this.src)
  
  def without(removeIndexBits : Int)         : this.type = {
    val toEmpty = (indexBits & removeIndexBits);
    if (toEmpty > 0) copy(
      x     = (if ((toEmpty & (1 << 0)) > 0) empty.x     else this.x    ),
      frame = (if ((toEmpty & (1 << 1)) > 0) empty.frame else this.frame),
      
      diff = toEmpty, emptied = toEmpty
    ) else this;
  }

  def assoc(src : ValueSource, root : ValueSource = src, eager:Boolean = false, indexFilter = 0x7FFFFFFF) = if (indexFilter == 0) empty else {
    var indexBits = this._voIndexSet & indexFilter;
    var diff = 0;

    val x = if ((indexFilter & 1 << 0) > 0) src[Int](0,"x", this.x) else empty.x;
    if (x != this.x) diff |= 1 << 0;

    var frame : Box = empty.frame;
    if (eager && (indexFilter & LAZY_MASK) > 0)
    {
      val frame = if ((indexFilter & 1 << 0) > 0) src[Int](0,"frame", this.frame) else empty.frame;
      if (frame != this.frame) diff |= 1 << 0;
    }
    //else: frame can be skipped, as it's lazy

    if (eager && diff == 0)
      this
    else
      new SpreadVO((indexBits | diff) & indexFilter, 0, root, x, frame)
  }

  {
    var indexBits = 0;
    val x = src[Int](0,"x");     if (src.containsKey(0,"x"))     indexBits |= 1;
    // ...
    indexBits &= indexes;

    new SpreadVO(indexBits, 0, this, x, frame)
  }
*/
}
