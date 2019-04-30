package commands
import files.{DirEntry, Directory}
import filesystem.State

class Mkdir(name: String) extends Command {


  def checkIllegal(name: String): Boolean = {
    name contains "."
  }

  def makeDirectory(name: String, state: State): State = {

    def updateStructure(currentDirectory: Directory, path: List[String],
                        newEntry: DirEntry): Directory = {
      println("Update was called")
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry: Directory = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd

    val allDirsInPath = wd.getAllFoldersInPath

    val newDir = Directory.empty(wd.path, name)

    val newRoot = updateStructure(state.root, allDirsInPath, newDir)

    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
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
