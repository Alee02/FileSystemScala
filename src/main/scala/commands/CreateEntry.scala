package commands
import filesystem.State

class CreateEntry(entryName: String) extends Command {
  override def apply(state: State): State = state
}
