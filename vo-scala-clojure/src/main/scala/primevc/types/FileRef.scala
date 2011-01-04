package primevc.types;
 import org.bson.types.ObjectId

class FileRef private[types](val ref:String)
{
  override def toString = ref
}

object FileRef
{
  def apply(s:String)  : FileRef = new FileRef(s)
  def apply(o:ObjectId) : FileRef = new FileRef(o.toString)
}
