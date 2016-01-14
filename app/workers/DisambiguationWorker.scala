package leon
package web
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
import leon.purescala.Types._
import leon.web.shared.Action
import leon.purescala.DefOps
import leon.synthesis.disambiguation._
import leon.purescala.Definitions.Program
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.grammars.{ValueGrammar, ExpressionGrammar}

class DisambiguationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._
  
  def convertExampleToFullCode(cstate: CompilationState, synth: Synthesizer, in: Expr, out: Expr): String = {
    val ci = synth.ci
    val SourceInfo(fd, pc, src, spec, tb) = ci

    leon.web.utils.FileInterfaceWeb.allCodeWhereFunDefModified(fd)(nfd => {
      val ea = new ExamplesAdder(synth.context, cstate.program)
      ea.addToFunDef(nfd, Seq((in, out)))
    })(cstate, synth.context)
  }
  
  def convertQuestionToJson(cstate: CompilationState, synth: Synthesizer, fd: FunDef, question: Question[Expr]): Map[String, JsValue] = {
    Map("input" -> toJson(tupleWrap(question.inputs).asString),
        "fname" -> toJson(fd.id.name),
        "confirm_solution" -> toJson(
            Map("display" -> question.current_output.asString(synth.program)(synth.context), 
                "allCode" -> convertExampleToFullCode(cstate, synth, tupleWrap(question.inputs), question.current_output))),
        "custom_alternative" -> toJson(
            Map("display" -> shared.Constants.disambiguationPlaceHolder, 
                "allCode" -> convertExampleToFullCode(cstate, synth, tupleWrap(question.inputs), StringLiteral(shared.Constants.disambiguationPlaceHolder)))),
        "alternatives" -> toJson(
            question.other_outputs.map(output =>
              Map("display" -> output.asString(synth.program)(synth.context), 
                  "allCode" -> convertExampleToFullCode(cstate, synth, tupleWrap(question.inputs), output)))))
  }
  
  def isGround(s: String): Boolean = {
    s == s.replaceAll(leon.synthesis.rules.StringRender.EDIT_ME, "")
  }
  
  def filterRedundantExprs(prev: Seq[Expr], current: Expr): Option[Expr] = {
    current match {
      case StringLiteral(currentStr) => 
        val currentStrSimp = currentStr.replaceAll(leon.synthesis.rules.StringRender.EDIT_ME, "")
        if(prev.forall { case StringLiteral(prevStr) =>
            val prevStrSimp = prevStr.replaceAll(leon.synthesis.rules.StringRender.EDIT_ME, "")
            val res = isGround(prevStr) || prevStrSimp != currentStrSimp
            res
          case _ => true})
          Some(current)
        else
          None
      case _ => Some(current)
    }
  }
  
  /** Specific enumeration of strings, which can be used with the QuestionBuilder#setValueEnumerator method */
  object NonEmptyValueGrammarfirst extends grammars.ExpressionGrammar[TypeTree] {
    def computeProductions(t: TypeTree)(implicit ctx: LeonContext): Seq[Gen] = t match {
       case StringType =>
          List(
            terminal(StringLiteral("foo")),
            terminal(StringLiteral("\"'\n\t")),
            terminal(StringLiteral("Lara 2007"))
          )
       case _ => ValueGrammar.computeProductions(t)
    }
  }
  
  def receive = {
    case OnUpdateCode(cstate) =>
      // Do nothing, wait for the synthesis to complete.

    case DoCancel =>
      sender ! Cancelled(this)

    case NewSolutions(cstate, synth, ssol) =>
      logInfo("Receiving new solutions ! disambiguating ...")
      event("disambiguation_started", Map())
      val ci = synth.ci
      val SourceInfo(fd, pc, src, spec, tb) = ci
      
      val qb = new QuestionBuilder(fd.paramIds, ssol, filterRedundantExprs)(synth.context, cstate.program)
      qb.setSortAlternativesBy(QuestionBuilder.AlternativeSortingType.BalancedParenthesisIsBetter())
      qb.setKeepEmptyAlternativeQuestions { case StringLiteral(s) if s.contains(leon.synthesis.rules.StringRender.EDIT_ME) => true case e => false }
      qb.setValueEnumerator(NonEmptyValueGrammarfirst)
      val questions = qb.result()
      
      if(questions.nonEmpty) {
        logInfo("Sending back results")
        event("disambiguation_result", convertQuestionToJson(cstate, synth, fd, questions.head))
      } else {
        event("disambiguation_noresult", Map())
      }
      logInfo("Finished disambiguating")
      
    case OnClientEvent(cstate, event) =>

    case _ =>
  }
}
