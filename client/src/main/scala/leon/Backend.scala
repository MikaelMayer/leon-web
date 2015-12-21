package leon.web.client
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ global => g, literal => l, newInstance => jsnew }
import leon.web.shared.{VerifStatus, TerminationStatus, InvariantStatus}
import leon.web.shared.{Module => ModuleName, Constants, Action}
import scala.language.dynamics

/**
 * @author Mikael
 */
object Backend {
  abstract class Module(val moduleName: String) {
    private def _send(msg: String): Unit = Main.leonSocket.send(msg)
    protected def _send(msg: js.Dynamic): Unit = _send(JSON.stringify(msg))
    val send = new scala.Dynamic {
      def applyDynamicNamed(method: String)(fields: (String, js.Any)*): Unit = {
        _send(l.applyDynamicNamed("apply")((fields.toSeq :+ ("module" -> (moduleName: js.Any))): _*))
      }
    }
  }
  
  object main extends Module(ModuleName.main) {
    def storePermaLink(code: String) =
      send(action = Action.storePermaLink, code = code)
    def accessPermaLink(link: String) = 
      send(action = Action.accessPermaLink, link = link)
    def setFeatureActive(feature: String, active: Boolean) =
      send(action = Action.featureSet, feature = feature, active = active)
    def doUpdateCode(code: String) =
      send(action = Action.doUpdateCode, code = code)
    def cancel() =
      send(action = Action.doCancel)
  }
  
  object synthesis extends Module(ModuleName.synthesis) {
    def getRulesToApply(fname: String, cid: Int) = 
      send(action = Action.getRulesToApply, fname = fname, cid = cid)
    def doApplyRule(fname: String, cid: Int, rid: Int) =
      send(action = Action.doApplyRule,  fname = fname, cid = cid, rid = rid)
    def explore(fname: String, cid: Int,
        path: js.Array[Int] = js.Array[Int](),
        exploreAction: String = "init",
        ws: Int = 0,
        select: Int = 0) =
      send(action = Action.doExplore,
           fname  = fname,
           cid = cid,
           exploreAction = exploreAction,
           path = path,
           select = select,
           ws = ws)
    def search(fname: String, cid: Int) =
      send(action = Action.doSearch, fname = fname, cid = cid)
  }
  
  object repair extends Module(ModuleName.repair) {
    def doRepair(fname: String) =
      send(action = Action.doRepair, fname = fname)
    def cancel() =
      send(action = Action.doCancel)
  }
  
  object verification extends Module(ModuleName.verification) {
    def prettyPrintCounterExample(output: String, rawoutput: String, fname: String) =
      send(action = Action.prettyPrintCounterExample, output = output, rawoutput = rawoutput, fname = fname)
  }
}
