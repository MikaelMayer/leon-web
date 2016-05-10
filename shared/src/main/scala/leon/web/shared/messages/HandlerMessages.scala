package leon.web
package shared
package messages
import shared.github._

sealed trait Message

/** Events triggered in reaction to the [[leon.web.client.react.Action]]s.
  * These events can be listened to, and are meant to trigger state
  * transformations can will themselves trigger a re-render of the app.
  */
sealed trait Event

case class HPermalink(
  link: String
) extends Message

case class HCommit(
  hash: String,
  shortHash: String,
  shortMessage: String,
  fullMessage: String,
  commitTime: String,
  author: String,
  committer: String,
  desc: String
)

case class HMoveCursor(line: Double) extends Message

case class HUpdateOverview(
  module: String,
  overview: HandlerMessages.DataOverView
) extends Message

case class SP(index: Int, line: Int, description: String)

case class SynthesisOverview(functions: Option[Map[String, Array[SP]]]) extends Message

case class HUpdateExplorationFacts(newFacts: Array[NewResult]) extends Message

case class NewResult(
  fromRow: Int,
  fromColumn: Int,
  toRow: Int,
  toColumn: Int,
  result: String
)

case class HEditor(
  annotations: Option[Array[CodeAnnotation]]
) extends Message

case class HNotification(
  content: String,
  `type`: String
) extends Message

case class HLog(
  message: String
) extends Message


case class HSynthesisResult(
  result: String,
  cid: Int,
  fname: String,
  problem: String,
  closed: Double,
  total: Double,
  solCode: String,
  allCode: String,
  cursor: Option[HMoveCursor]
) extends Message

case class HDisambiguationDisplay(
  var display: String,
  allCode: String
)

case object DisambiguationStarted extends Message

case object DisambiguationNoresult extends Message

case class HDisambiguationResult(
  input: String,
  fname: String,
  confirm_solution: HDisambiguationDisplay,
  custom_alternative: HDisambiguationDisplay,
  alternatives: List[HDisambiguationDisplay]
) extends Message

case class HSynthesisExploration(
  html: String,
  fname: String,
  cid: Int,
  from: Array[String],
  allCode: String,
  cursor: Option[HMoveCursor]
) extends Message

case class HRulesApps(
  status: String,
  id: Int,
  name: String
) extends Status 

case class HSynthesisRulesToApply(
  fname: String,
  cid: Int,
  rulesApps: Array[HRulesApps]
) extends Message

case class HRepairResult(
  result: String = "",
  progress: String = "",
  error: String = "",
  focused: String = "",
  success: String = "",
  solCode: String = "",
  allCode: String = "",
  cursor: Option[HMoveCursor] = None
) extends Message

case class ResultOutput(
  result: String,
  output: DualOutput
)

case class DualOutput(rawoutput: String, prettyoutput: String, var modifying: Option[String])

case class VC(
  status: String,
  fun: String,
  kind: String,
  time: String,
  counterExample: Option[Map[String, DualOutput]],
  execution: Option[ResultOutput]
) extends Status

case class HReplaceCode(newCode: String) extends Message

case class HCompilationProgress(total: Float, current: Float) extends Message

case class HCompilation(status: String) extends Message with Status

case class StatusCode(
  status: String,
  code: String
) extends Status 

trait Status{ def status: String }

case class VerificationDetails(
  status: String,
  vcs: Array[VC]
) extends Message with Status

case class TerminationDetails(
  status: String,
  call: String,
  calls: Array[String],
  reason: Option[String]
) extends Status 

case class InvariantDetails(
  status: String,
  fun: String,
  oldInvariant: String,
  newInvariant: String,
  newCode: String,
  time: Double
)

case class HInvariants(
  module: String,
  overview: Map[String, InvariantDetails],
  kind: String,
  code: String
) extends Message

case class OverviewFunction(
  name: String,
  displayName: String,
  line: Int,
  column: Int
)

case class RepositoriesLoaded (
  repos: Array[Repository]
) extends Message with Event

case class RepositoryLoaded(
  repository: Repository,
  files: Array[String],
  branches: Array[Branch],
  currentBranch: String
) extends Message with Event

case class FileLoaded(
  file: String,
  content: String
) extends Message with Event

case class BranchChanged(
  success: Boolean,
  branch: Option[String],
  files: Option[Array[String]],
  error: Option[String]
) extends Message with Event
//case class FileLoaded(fileName: String, content: String) extends Event


//case class BranchChanged(branch: String, files: Seq[String]) extends Message with Event
case class CodeUpdated(code: String) extends Message with Event
//case class GitProgress(task: String, percentage: Option[String]) extends Message with Event

case class GitProgress(
  taskName: String,
  status: String,
  percentage: Option[String]
) extends Message with Event with Status 

sealed trait GitOperationResult
case class GitStatusDiff(
  status: Map[String, Set[String]],
  diff: String
) extends GitOperationResult
case object GitOperationResultNone extends GitOperationResult
case class GitCommits(
  commits: Seq[Commit]
) extends GitOperationResult
case class Commit(
  hash: String,
  shortHash: String,
  shortMessage: String,
  fullMessage: String,
  commitTimeStr: String,
  author: String,
  committer: String,
  desc: String
)

case class GitOperationDone(
  op: GitOperation,
  success: Boolean,
  data: GitOperationResult = GitOperationResultNone
) extends Message with Event
  
object HandlerMessages {
  type VCS = Array[VC]
  type Html = String
  type DataOverView = Map[String, OverviewFunction]
}
  
object Picklers {
  import boopickle.Default._
  implicit val annotationPickler = generatePickler[CodeAnnotation]
  implicit val gopPickler = generatePickler[GitOperationDone]
  implicit val vcPickler = generatePickler[VC]
  implicit val msgPickler = generatePickler[Message]
}