package files

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry])
  extends DirEntry(parentPath, name) {

  def getAllFoldersInPath(): List[String] = {
    path.substring(1).split(Directory.SEPARATOR).toList
  }

  def hasEntry(name: String): Boolean = {
    ???
  }
  def findDescendant(path: List[String]): Directory = ???

}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory = {
    new Directory(parentPath, name, List())
  }
}