package prime.types;
 import prime.utils.ConvertTo;
  import java.io._

trait FileRefOutputStream extends OutputStream {
  /** Closes the OutputStream and returns the FileRef constructed */
  def ref: FileRef
}

trait FileRepository {
  def exists (ref : FileRef): Boolean
  def toURI(instance : FileRef): URI
  def create (): FileRefOutputStream
  def absorb (file : File): FileRef
  def stream (ref : FileRef): InputStream

  def store[T](writer: FileRefOutputStream => T): FileRef = {
    val bldr = create
    writer(bldr)
    bldr.ref
  }
}

trait LocalFileRepository extends FileRepository {
  def getFile(instance : FileRef): File
  def apply  (instance : FileRef): File = this getFile instance
  def stream (instance : FileRef): InputStream = new FileInputStream(this(instance))
}

class BasicLocalFileRepository(val root:File) extends LocalFileRepository {
  root.mkdir();
  require(root.isDirectory, root + " is not a directory.")

  def toURI   (f : FileRef) = ConvertTo.uri(f.toString)
  def getFile (f : FileRef) = new File(root.getAbsolutePath + "/" + f.toString)
  def exists  (f : FileRef) = getFile(f).exists

//  def save (stream : OutputStream) = FileRef(new StreamWrapper(stream)).get

  def absorb (file : File) = {
    val ref = FileRef(file)
    val newFile = getFile(ref)
    if (newFile.exists) {
      org.apache.commons.io.FileUtils.touch(newFile)
      file.delete
    }
    else file renameTo newFile
    assert(newFile.exists, "Absorbing "+ file.getAbsolutePath +" failed. Target: " + newFile.getAbsolutePath);
    ref
  }

  def create = new TmpFileStream()

  class TmpFileStream private[prime](tmpFile : File, out : FileOutputStream) extends FilterOutputStream(out) with FileRefOutputStream
  {
    def this(tmpFile : File) = this(tmpFile, new FileOutputStream(tmpFile))
    def this() = this(File.createTempFile("blfr-", ".tmp", root))
    
    protected val builder = FileRef(this)

    def ref = {
      val ref = builder.get
      tmpFile renameTo getFile(ref)
      ref
    }
  }
}
