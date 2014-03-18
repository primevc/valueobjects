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

class BasicLocalFileRepositorySpec extends Specification {

  /* Initialisation. */

  val repo = new BasicLocalFileRepository(systemTmpDir)

  /* The actual tests. */

  "The basic local repository" should {

    "store the contents using an outputstream" in {
      val (tmpData, base64) = createTempData
      val writer = { out: OutputStream => IOUtils.write(tmpData, out) }
      val ref = repo.store(writer)
      ref.toString mustEqual base64
    }

    "store the contents of an inputstream" in {
      val (tmpData, base64) = createTempData
      val is = new ByteArrayInputStream(tmpData)
      val ref = repo.absorb(is)
      ref.toString mustEqual base64
    }

    "delete from the repository" in {
      val (tmpData, base64) = createTempData
      val is = new ByteArrayInputStream(tmpData)
      val ref = repo.absorb(is)
      val file = new File(systemTmpDir + "/" + ref.toString)

      repo.exists(ref) must beTrue
      file.exists must beTrue
      repo.delete(ref)
      repo.exists(ref) must beFalse
      file.exists must beFalse
    }
  }
}
