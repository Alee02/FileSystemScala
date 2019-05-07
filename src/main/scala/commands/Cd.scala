package commands
import files.{DirEntry, Directory}
import filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  def findEntry(root: Directory, path: String): DirEntry = {

    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if(nextDir == null) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // eliminate/collapse relative tokens.
    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {

      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if(result.isEmpty) Nil
        else collapseRelativeTokens(path.tail, result.tail)
      } else {
        collapseRelativeTokens(path.tail, result :+ path.head)
      }
    }
t add
    val newTokens = collapseRelativeTokens(tokens, List())

    findEntryHelper(root, newTokens)

  }

  override def apply(state: State): State = {


    // 1. find root
    val root = state.root
    val wd = state.wd
    // 2. find the absolute path of the directory I want to cd to
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    // 3. find the directory to cd to, given the path#

    val destinationDirectory = findEntry(root, absolutePath)
    /// 4. change the state given the new directory.
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory")
    else State(root, destinationDirectory.asDirectory)
  }
}
