package prime.types

import java.net.InetAddress
import java.io.{File, OutputStream, ByteArrayInputStream}
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.{FileUtils, IOUtils}
import org.specs2.mutable._
import scala.util.Random

object SpecHelper {

  val systemTmpDir = new File(System.getProperty("java.io.tmpdir"))

  /** Create a little random data string and return it, together with the
    * Base64 encoding of the SHA256 hash of that data.
    */
  def createTempData = {
    val tmpData = new Array[Byte](100)
    Random.nextBytes(tmpData)
    val hash = DigestUtils.sha256(tmpData)
    val base64 = Base64.encodeBase64URLSafeString(hash)
    (tmpData, base64)
  }
}


import SpecHelper._

class NFSRepositorySpec extends Specification {

  /* Initialisation. */

  val mountsRoot = new File(systemTmpDir, "mounts")
  mountsRoot.mkdir

  val myHostname = InetAddress.getLocalHost().getHostName();
  val myMount = new File(mountsRoot, myHostname)
  myMount.mkdir

  val otherMount = new File(mountsRoot, "other-host")
  otherMount.mkdir

  val repo: FileRepository = new NFSRepository(mountsRoot, "test-repo")

  /* The actual tests. */

  "The NFS repository" should {

    "absorb a file in its own mount" in {

      val (tmpData, base64) = createTempData
      val tmpFile = File.createTempFile("nfs", ".test")
      FileUtils.writeByteArrayToFile(tmpFile, tmpData)
      val ref = repo.absorb(tmpFile)

      "return a correct FileRef" in {
        ref.toString mustEqual "nfs://" + myHostname + "/test-repo/" + base64
      }

      "contain the absorbed file" in {
        repo.exists(ref)
      }

      "determine the URI for a FileRef" in {
        val uri = repo.toURI(ref)
        (uri.getHost mustEqual myHostname) and
          (uri.getPath mustEqual "/test-repo/" + base64) and
          (uri.getScheme mustEqual "nfs")
      }

      "stream the contents of a FileRef" in {
        val inputStream = repo.stream(ref)
        val streamContent = IOUtils.toByteArray(inputStream)
        streamContent mustEqual tmpData
      }
    }

    "store the contents using an outputstream" in {
      val (tmpData, base64) = createTempData
      val writer = { out: OutputStream => IOUtils.write(tmpData, out) }
      val ref = repo.store(writer)
      ref.toString mustEqual "nfs://" + myHostname + "/test-repo/" + base64
    }

    "store the contents of an inputstream" in {
      val (tmpData, base64) = createTempData
      val is = new ByteArrayInputStream(tmpData)
      val ref = repo.absorb(is)
      ref.toString mustEqual "nfs://" + myHostname + "/test-repo/" + base64
    }
  }
}
