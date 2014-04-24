package prime.types

import java.io._
import java.net.InetAddress


/** This repository is used for reading and writing to NFS file repositories.
  *
  * It recognises and creates `nfs:` [[prime.types.FileRef]] objects. The idea
  * is that files are not distributed among the servers/hosts, but are loaded
  * from the host the file resides on.
  *
  * The [[prime.types.FileRef]] prefix for this repository follows the following
  * convention: `nfs:internal-hostname//repository-name/`. A file can be read
  * and written to the path `/nfsMountRoot/internal-hostname/repository-name/HASH`.
  *
  * @param nfsMountsRoot A file pointing to the root directory of all mount
  *                      points accessible by this host.
  * @param repositoryName The name of this repository.
  */
class NFSRepository(nfsMountsRoot: File, repositoryName: String)
extends LocalFileRepository {

  // Initialisation.

  require(nfsMountsRoot.exists && nfsMountsRoot.isDirectory,
    "The root directory of all NFS mounts must exist and point to a directory.")

  val myLocalHostname = InetAddress.getLocalHost().getHostName();
  val myNFSMount = new File(nfsMountsRoot, myLocalHostname)

  require(myNFSMount.exists && myNFSMount.isDirectory,
    "The NFS \"mount\" for this host, which should be located at '" +
    myNFSMount.getAbsolutePath + "', does not exist or is not a directory.")

  val root = new File(myNFSMount, repositoryName)
  root.mkdir

  require(root.exists && root.isDirectory,
    "The local repository directory, which should be located at '" +
    root.getAbsolutePath + "', could not be created.")


  // Remove temporary files older than a day.

  val filter = new FileFilter {
    def accept(f: File) = f.isFile &&
                          f.getName.endsWith(".tmp") &&
                          f.lastModified < System.currentTimeMillis - 1000 * 60 * 60 * 24
  }
  root.listFiles(filter).foreach( f => f.delete() )


  // Overriding definitions.

  val prefix = "nfs://" + myLocalHostname + "/" + repositoryName + "/"

  def getFile (fileRef: FileRef) = {
    val fileRefURI = toURI(fileRef)
    require(fileRefURI.getScheme == "nfs", "Cannot read a non-NFS FileRef ("+ fileRefURI +") from an NFS repository.")
    val filePath = nfsMountsRoot.getAbsolutePath + "/" + fileRefURI.getHost +
                   "/" + fileRefURI.getPath
    new File(filePath)
  }
}
