package commands
import files.{DirEntry, Directory}
import filesystem.State

class Mkdir(name: String) extends Command {

  def updateStructure(currentDirectory: Directory, path: List[String],
                      newEntry: DirEntry) = {
    
  }
  def checkIllegal(name: String): Boolean = {
    name contains "."
  }

  def makeDirectory(name: String, state: State): State = {
    val wd = state.wd
    val fullPath = wd.path

    val allDirsInPath = wd.getAllFoldersInPath()

    val newDir = Directory.empty(wd.path, name)

    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    val newWd = newRoot.findDescendant(allDirsInPath)
  }

  override def apply(state: State): State = {
    val wd = state.wd
    if(wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      // TODO mkdir -p something/somethingElse
      state.setMessage(s"$name must not contains separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(s"$name: illegal entry name!")
    } else {
      makeDirectory(name, state)
    }
  }
}
