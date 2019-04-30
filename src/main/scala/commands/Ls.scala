package commands
import files.DirEntry
import filesystem.State

class Ls extends Command {

  def createFormattedOutput(contents: List[DirEntry]): String = {
    if(contents.isEmpty) ""
    else {
      val entry = contents.head
      "/" + entry.name + " [" + entry.getType + "]"  + " \n" + createFormattedOutput(contents.tail)
    }
  }

  override def apply(state: State): State = {
    val contents = state.wd.contents
    val niceOutput: String = createFormattedOutput(contents)
    state.setMessage(niceOutput)
  }
}
