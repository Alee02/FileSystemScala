package files

import filesystem.FilesystemException

class File(override val parentPath: String, override val name: String, contents: String) extends DirEntry(parentPath, name) {
  override def asDirectory: Directory =  throw new FilesystemException("A file cannot be converted to a diredctory!")

  override def getType: String = "File"

  override def asFile: File = this

  override def isDirectory: Boolean = false

  override def isFile: Boolean = true
}

object File {
  def empty(parentPath: String, name: String): File = {
    new File(parentPath, name, "")
  }

}
