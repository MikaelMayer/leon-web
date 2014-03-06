package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.purescala.Trees._
import leon.purescala.Definitions.TypedFunDef

class TutorWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._
  import leon.codegen._
  import leon.evaluators._

  val reporter = new WorkerReporter(session)
  var ctx      = leon.Main.processOptions(Nil).copy(interruptManager = interruptManager, reporter = reporter)

  def executeStep(cstate: CompilationState, line: Int) = {
    cstate.optProgram match {
      case Some(p) =>
        p.definedFunctions.find(d => !d.annotations("verified") && d.getPos.line == line+1) match {
          case Some(fd) =>
            notifyError(fd.id.toString)

          case None =>
            notifyError("No function found at line "+(line+1))
        }
      case None =>
    }
  }

  def receive = {
    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case "executeStep" =>
          executeStep(cstate, (event \ "line").as[Int])

        case action =>
          notifyError("Received unknown action: "+action)
      }

    case _ =>
  }
}
