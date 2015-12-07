package leon.web
package workers

import akka.actor._
import play.api.libs.json.Json._
import scala.concurrent.duration._
import models._
import leon.LeonContext
import leon.utils._
import leon.purescala.PrinterOptions
import leon.purescala.PrinterContext
import leon.purescala.ScalaPrinter
import leon.purescala.Definitions.FunDef
import leon.synthesis._
import leon.purescala.Common._
import leon.purescala.ExprOps._
import leon.purescala.Constructors._
import leon.purescala.Expressions._
import leon.web.shared.Action
import leon.purescala.DefOps
import leon.synthesis.disambiguation._
import play.api.libs.json._
import play.api.libs.json.Json._

class DisambiguationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._

  def convertExampleToFullCode(cstate: CompilationState, synth: Synthesizer, in: Expr, out: Expr): String = {
    import leon.purescala.PrinterHelpers._

    val ci = synth.ci
    val SourceInfo(fd, pc, src, spec, tb) = ci


    val fInt = new FileInterface(new MuteReporter())

    val nfd = fd.duplicate()
    
    val ea = new ExamplesAdder(synth.context, cstate.program)
    
    ea.addToFunDef(nfd, Seq((in, out)))

    val fds = nfd :: Nil

    val prog = DefOps.replaceFunDefs(cstate.program)(f => if(f == fd) Some(nfd) else None)
    val p = new ScalaPrinter(PrinterOptions(), Some(prog._1))

    val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                  fd,
                                  (indent) => {
      implicit val pctx = PrinterContext(fd, Nil, indent, p)
      p"${nfd}"
      p.toString
    })

    allCode
  }
  
  def convertQuestionToJson(cstate: CompilationState, synth: Synthesizer, fd: FunDef, question: Question[Expr]): Map[String, JsValue] = {
    Map("input" -> toJson(tupleWrap(question.inputs).asString),
        "fname" -> toJson(fd.id.name),
        "confirm_solution" -> toJson(
            Map("display" -> question.current_output.asString(synth.program)(synth.context), 
                "allCode" -> convertExampleToFullCode(cstate, synth, tupleWrap(question.inputs), question.current_output))),
        "alternatives" -> toJson(
            question.other_outputs.map(output =>
              Map("display" -> output.asString(synth.program)(synth.context), 
                  "allCode" -> convertExampleToFullCode(cstate, synth, tupleWrap(question.inputs), output)))))
  }
  
  def receive = {
    case OnUpdateCode(cstate) =>
      // Do nothing, wait for the synthesis to complete.

    case DoCancel =>
      sender ! Cancelled(this)

    case NewSolutions(cstate, synth, ssol) =>
      logInfo("Receiving new solutions ! disambiguating ...")
      val ci = synth.ci
      val SourceInfo(fd, pc, src, spec, tb) = ci
      
      val qb = new QuestionBuilder(fd.paramIds, ssol, expr => expr match {
        case StringLiteral(s) => if(!s.contains(leon.synthesis.rules.StringRender.EDIT_ME)) Some(expr) else None
        case _ => Some(expr)
      })(synth.context, cstate.program)
      
      val questions = qb.result()
      
      if(questions.nonEmpty) {
        logInfo("Sending back results")
        event("disambiguation_result", convertQuestionToJson(cstate, synth, fd, questions.head))
      }
      logInfo("Finished disambiguating")
      
    case OnClientEvent(cstate, event) =>

    case _ =>
  }
}
