package leon.web
package models

import akka.actor._
import akka.util.duration._
import akka.dispatch.Await
import akka.dispatch.Future

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import akka.util.Timeout
import akka.pattern.ask

import play.api.Play.current

import leon.{LeonContext, Settings, Reporter, SilentReporter}
import leon.solvers.z3.{UninterpretedZ3Solver, FairZ3Solver}
import leon.solvers.{Solver, TimeoutSolver, TrivialSolver}
import leon.plugin.{TemporaryInputPhase, ExtractionPhase}
import leon.synthesis._
import leon.synthesis.search._
import leon.synthesis.utils.SynthesisProblemExtractionPhase
import leon.verification._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.ScalaPrinter
import leon.purescala.EquivalencePrettyPrinter
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.xlang._

import java.util.concurrent.TimeoutException

class ConsoleSession(remoteIP: String) extends Actor {
  import context.dispatcher
  import ConsoleProtocol._

  var channel: PushEnumerator[JsValue] = _
  var reporter: WSReporter = _

  var codeHistory: List[String]        = Nil
  var compilationHistory: List[String] = Nil

  var chooses: Map[Int, (ChooseInfo, SimpleSearch)] = Map()
  var currentProgram: Option[Program] = None
  var verificationCtx: Option[VerificationContext] = None
  var imperativeInfo: (Set[FunDef], Map[FunDef, FunDef], Map[FunDef, FunDef]) = (Set(), Map(), Map());

  var verifOverview = Map[FunDef, List[VerificationCondition]]()

  case class FunctionHash(fd: FunDef) {
    val v = {
      val pp = new EquivalencePrettyPrinter()
      pp.pp(fd, 0)
      pp.toString
    }

    override def equals(o: Any): Boolean = {
      o match {
        case fh: FunctionHash =>
          fh.v == v
        case _ =>
          false
      }
    }

    override def hashCode: Int = {
      v.##
    }
  }

  def innerFunctionsOf(fd: FunDef): Set[FunDef] = {
    val (_, _, parents) = imperativeInfo

    parents.flatMap((p: (FunDef, FunDef)) => p match {
      case (child, parent) => if(parent == fd) List(child) else List()
    }).toSet
  }

  def origFunctionFor(fd: FunDef): FunDef = {
    val (_, freshFunDefs, _) = imperativeInfo
    val originalFunDefs = freshFunDefs.map(x => (x._2, x._1))

    originalFunDefs.getOrElse(fd, fd)
  }


  def resetContext() {
    chooses = Map()
    currentProgram = None
    verifOverview = Map()
    verificationCtx = None
    imperativeInfo = (Set(), Map(), Map())
  }

  def notifyError(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("error")))
  }

  def vcToJson(vc: VerificationCondition): JsValue = {
    def ceToJson(ce: Map[Identifier, Expr]): JsValue = {
      toJson(ce.map{ case (id, ex) =>
        id.toString -> toJson(ScalaPrinter(ex))
      })
    }

    val base = Map(
      "kind"   -> toJson(vc.kind.toString),
      "fun"    -> toJson(origFunctionFor(vc.funDef).id.name),
      "status" -> toJson(vc.status),
      "time"   -> toJson(vc.time.map("%-3.3f".format(_)).getOrElse("")))

    vc.counterExample match {
      case Some(ce) =>
        toJson(base + ("counterExample" -> ceToJson(ce)))
      case None =>
        toJson(base)
    }
  }

  def getOverallVCsStatus(vcs: Seq[VerificationCondition]) = {
    var c: Option[String] = None

    for (vc <- vcs if vc.hasValue) {
      if (vc.value == Some(true) && c == None) {
        c = Some("valid")
      }
      if (vc.value == Some(false)) {
        c = Some("invalid")
      }
      if (vc.value == None && c != Some("invalid")) {
        c = Some("timeout")
      }
    }
    if (vcs.isEmpty) {
      c = Some("valid")
    }

    c.getOrElse("undefined")
  }

  def notifyOverview() {
    case class FunVerif(fd: FunDef, vcs: List[VerificationCondition]) {
      val totalTime = vcs.flatMap(_.time).foldLeft(0d)(_ + _)
      var status = getOverallVCsStatus(vcs)
    }


    var verifResults = verifOverview.map { case (fd, vcs) =>
      (fd, FunVerif(fd, vcs))
    }

    for ((fd, fv) <- verifResults if fv.status != "valid") {
      for (cfd <- currentProgram.get.transitiveCallers(fd) if verifResults.get(cfd).map(_.status) == Some("valid")) {
        verifResults(cfd).status = "cond-valid"
      }
    }

    val fvcs = verifResults.toSeq.sortWith{ (a,b) => a._1 < b._1 }.map{ case (fd, fv) =>
      toJson(Map(
        "name" -> toJson(origFunctionFor(fv.fd).id.name),
        "status" -> toJson(fv.status),
        "time" -> toJson(fv.totalTime),
        "vcs"  -> toJson(fv.vcs.map(vcToJson))
      ))
    }

    event("verification_overview", Map("overview" -> toJson(fvcs.toSeq)))
  }

  def notifySuccess(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("success")))
  }

  def logInfo(msg: String) {
    Logger.info("["+remoteIP+"] "+msg)
  }

  def logInfo(msg: String, t: Throwable) {
    Logger.info("["+remoteIP+"] "+msg, t)
  }

  def clientLog(msg: String) = {
    logInfo("[>] L: "+msg)
    channel.push(toJson(Map("kind" -> "log", "level" -> "log", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    logInfo("[>] "+kind)
    channel.push(toJson(Map("kind" -> toJson(kind)) ++ data))
  }

  def setAnnotations(as: Seq[CodeAnnotation]) {
    event("editor", Map("annotations" -> toJson(as.map(_.toJson))))
  }

  def receive = {
    case Init =>
      channel  = Enumerator.imperative[JsValue]()
      reporter = new WSReporter(channel)
      sender ! InitSuccess(channel)
      logInfo("New client")

    case ProcessClientEvent(event) =>
      try {
        logInfo("[<] "+(event \ "action").as[String])

        (event \ "action").as[String] match {
          case "doUpdateCode" =>
            self ! UpdateCode((event \ "code").as[String])

          case "storePermaLink" =>
            self ! StorePermaLink((event \ "code").as[String])

          case "accessPermaLink" =>
            self ! AccessPermaLink((event \ "link").as[String])

          case "synthesis_getRulesToApply" =>
            val chooseLine   = (event \ "chooseLine").as[Int]
            val chooseColumn = (event \ "chooseColumn").as[Int]

            self ! SynthesisGetRulesToApply(chooseLine, chooseColumn)

          case "synthesis_doApplyRule" =>
            val chooseId = (event \ "cid").as[Int]
            val ruleId = (event \ "rid").as[Int]

            self ! SynthesisApplyRule(chooseId, ruleId)

          case "synthesis_doSearch" =>
            val chooseId = (event \ "cid").as[Int]

            self ! SynthesisSearch(chooseId)

          case "synthesis_doCancelSearch" =>
            val chooseId = (event \ "cid").as[Int]

            self ! SynthesisCancelSearch(chooseId)

          case "verification_doVerify" =>
            val fname = (event \ "fname").as[String]
            logInfo("Verifying "+fname+"...")

            self ! VerificationDoManualVerify(fname)

          case "verification_doCancel" =>
            self ! VerificationDoCancelCurrent

          case action =>
            notifyError("Received unknown action: "+action)
        }
      } catch {
        case t: Throwable =>
          notifyError("Could not process event: "+t.getMessage)
      }

    case VerificationDoCancelCurrent =>
      if (verificationCtx.isDefined) {
        logInfo("Cancelling verification..")
        verificationCtx.get.shouldStop.set(true)
        verificationCtx.get.solvers.map(_.halt)
      } else {
        logInfo("Cannot cancel unknown verification!")
      }

    case VerificationDone =>
      if (verificationCtx.isDefined) {
        logInfo("Verification finished")
        verificationCtx = None
      } else {
        logInfo("Cannot finish unknown verification!")
      }

    case VerificationDoManualVerify(fname) =>
      val (wasLoop, freshFunDefs, parents) = imperativeInfo

      val realfname = freshFunDefs.find(_._1.id.name == fname) match {
        case Some((fd1, fd2)) => fd2.id.name
        case None => fname
      }

      verifOverview.keySet.find(_.id.name == realfname) match {
        case Some(fd) =>
          self ! VerificationDoVerify((Set(fd) ++ innerFunctionsOf(fd)).map(_.id.name), true)
        case None =>
          logInfo("Fanction nowt found!")
      }

    case VerificationDoVerify(toAnalyze, standalone) =>
      if (currentProgram.isDefined && verificationCtx.isEmpty) {

        val verifTimeout = 3000L // 3sec

        //val reporter = if (standalone) this.reporter else new SilentReporter

        var compContext  = leon.Main.processOptions(reporter, List("--feelinglucky", "--evalground"))

        val (wasLoop, freshFunDefs, parents) = imperativeInfo

        def functionWasLoop(fd: FunDef): Boolean =
          wasLoop.contains(origFunctionFor(fd))

        val solvers = List(
          new TimeoutSolver(new TrivialSolver(compContext), verifTimeout),
          new TimeoutSolver(new FairZ3Solver(compContext), verifTimeout)
        )

        solvers.map(_.setProgram(currentProgram.get))

        val vctx = VerificationContext(compContext, solvers, reporter)
        verificationCtx = Some(vctx)

        val vcs = verifOverview.collect {
          case (fd, vcs) if toAnalyze(fd.id.name) => fd -> vcs
        }

        val future = Future {
          try {
            val vr = AnalysisPhase.checkVerificationConditions(vctx, vcs)

            val newVR = XlangAnalysisPhase.completeVerificationReport(vr, parents, functionWasLoop _)

            Some(newVR)

          } catch {
            case t: Throwable =>
              logInfo("[!] Verification crashed!", t)
              None
          }
        }

        future.map {
          case Some(report) =>
            try {
              val status = getOverallVCsStatus(report.conditions)

              self ! VerificationDone

              for ((f, vcs) <- report.fvcs) {
                verifOverview += f -> vcs
              }

              notifyOverview()

              if (standalone) {
                event("verification_result", Map("status" -> toJson(status), "vcs" -> toJson(report.conditions.map(vcToJson))))
              }
            } catch {
              case t: Throwable =>
                self ! VerificationDone
                if (standalone) {
                  event("verification_result", Map("status" -> toJson("failure"), "vcs" -> toJson(List[String]())))
                }
                logInfo("[!] Solution Processing crashed", t)
            }
          case None =>
            self ! VerificationDone
            if (standalone) {
              event("verification_result", Map("status" -> toJson("failure"), "vcs" -> toJson(List[String]())))
            }
            logInfo("Verificatoin did not finish")
        }
      } else {
        if (standalone) {
          event("verification_result", Map("status" -> toJson("failure")))
        }
        if (!currentProgram.isDefined) {
          notifyError("No program available. Compilation failed?")
        } else if (verificationCtx.isDefined) {
          logInfo("Verification already running ?!?")
        }
      }

    case StorePermaLink(code) =>
      Permalink.store(code) match {
        case Some(link) =>
          event("permalink", Map("link" -> toJson(link)))
        case _ =>
          notifyError("Coult not create permalink")
      }

    case AccessPermaLink(link) =>
      Permalink.get(link) match {
        case Some(code) =>
          event("replace_code", Map("newCode" -> toJson(code)))
        case None =>
          notifyError("Link not found ?!?: "+link)
      }

    case UpdateCode(code) =>
      if (codeHistory.headOption != Some(code)) {
        clientLog("Compiling...")
        logInfo("Code to compile:\n"+code)

        codeHistory = code :: codeHistory

        val compReporter = new CompilingWSReporter(channel)
        var compContext  = leon.Main.processOptions(compReporter, Nil)
        var synthContext = compContext.copy(reporter = reporter)

        val pipeline = TemporaryInputPhase andThen ExtractionPhase

        val oldVerifOverView = verifOverview
        resetContext()

        try {
          val pgm = pipeline.run(compContext)((code, Nil))

          val pgm1 = ArrayTransformation(compContext, pgm)
          val pgm2 = EpsilonElimination(compContext, pgm1)
          val (pgm3, wasLoop) = ImperativeCodeElimination.run(compContext)(pgm2)
          val (program, parents, freshFunDefs) = FunctionClosure.run(compContext)(pgm3)

          currentProgram = Some(program)
          imperativeInfo = (wasLoop, freshFunDefs, parents)

          def noop(u:Expr, u2: Expr) = u

          var options = SynthesisOptions()
          options = options.copy(cegisGenerateFunCalls = true)
          val ctx = new LeonContext()

          // Extract Synthesis Problems
          println(ScalaPrinter(program))
          chooses = ChooseInfo.extractFromProgram(synthContext, program, options).zipWithIndex.map {
            case (ci, i) =>
              val search = new SimpleWebSearch(this, ci.synthesizer, ci.problem)
              (i+1) -> (ci, search)
          }.toMap

          // Extract Verification Problems
          val verifFunctions = if (chooses.isEmpty) {
            program.definedFunctions.filter(fd => fd.hasBody).toSet
          } else {
            Set()
          }

          event("compilation", Map("status" -> toJson("success")))

          val synthesisAnnotations = chooses.values.map { case (ci, _) =>
            val (line, col) = ci.ch.posIntInfo
            CodeAnnotation(line, col, "Synthesis problem: "+ci.problem.toString, CodeAnnotationSynthesis)
          }

          val verificationAnnotations = verifFunctions.map { case fd =>
            val (line, col) = fd.posIntInfo
            CodeAnnotation(line, col, "Verifiable function", CodeAnnotationVerification)
          }
          setAnnotations((synthesisAnnotations ++ verificationAnnotations).toSeq)

          clientLog("Compilation successful!")

          var toGenerate = Set[FunDef]()

          // Generate VCs
          for (f <- verifFunctions) {
            val h = FunctionHash(f)

            oldVerifOverView find { case (fd, _) => FunctionHash(fd) == h } match {
              case Some((fd, vcs)) =>
                verifOverview += f -> vcs
              case None =>
                verifOverview -= f
                toGenerate += f
            }
          }

          val toInvalidate = toGenerate.flatMap { program.transitiveCallers _ }

          toGenerate ++= toInvalidate

          if (!toGenerate.isEmpty) {
            clientLog("Generating VCs...")

            toGenerate.foreach{ fd =>
              toGenerate ++= innerFunctionsOf(fd)
            }

            // Generate VCs
            val fvcs = AnalysisPhase.generateVerificationConditions(reporter, program, toGenerate.map(_.id.name))

            for ((f, vcs) <- fvcs) {
              verifOverview += f -> vcs
            }

            clientLog("Done!")

            notifyOverview()
          }

          self ! VerificationDoVerify(toGenerate.map(_.id.name), false)

          compilationHistory = "success" :: compilationHistory

        } catch {
          case t: Throwable =>
            chooses = Map()
            currentProgram = None

            verifOverview = oldVerifOverView

            logInfo("Compilation failed: "+t.getMessage)
            for ((l,e) <- compReporter.errors) {
              logInfo("  "+e.mkString("\n  "))
            }

            event("compilation", Map("status" -> toJson("failure")))

            setAnnotations(compReporter.errors.map{ case (l,e) =>
              CodeAnnotation(l, 0, e.mkString("\n"), CodeAnnotationError)
            }.toSeq)

            compilationHistory = "failure" :: compilationHistory

            clientLog("Compilation failed!")

        }
      } else {
        compilationHistory.headOption.foreach { status =>
          event("compilation", Map("status" -> toJson(status)))
        }
      }

    case Quit =>

    case SynthesisCancelSearch(cid) =>
      chooses.get(cid) match {
        case Some((ci, search)) =>
          logInfo("Cancelling synthesis search")
          ci.synthesizer.shouldStop.set(true)
          search.stop()
        case None =>
      }

    case SynthesisSearch(cid) =>
      try {
        chooses.get(cid) match {
          case Some((ci @ ChooseInfo(ctx, prog, fd, _, ch, _), search)) =>

            val future = Future {
              try {
                ci.synthesizer.shouldStop.set(false)
                val sol = search.search()
                sol
              } catch {
                case t: Throwable =>
                  notifyError("Woops, search crashed: "+t.getMessage)
                  logInfo("Synthesis search crashed", t)
                  None
              }
            }

            future.map {
              case Some((sol, isTrusted)) =>
                try {
                  val (newSol, succeeded) = if (!isTrusted) {
                    // Validate solution
                    event("synthesis_proof", Map("status" -> toJson("started")))
                    ci.synthesizer.validateSolution(search, sol, 2000L) match {
                      case (sol, true) =>
                        event("synthesis_proof", Map("status" -> toJson("success")))
                        (sol, true)
                      case (sol, false) =>
                        event("synthesis_proof", Map("status" -> toJson("failure")))
                        (sol, false)
                    }
                  } else {
                    (sol, true)
                  }

                  if (succeeded) {
                    val chToSol = Map(ci -> newSol.toSimplifiedExpr(ctx, prog))
                    val fInt = new FileInterface(reporter)

                    val newCode = fInt.substitueChooses(codeHistory.head, chToSol, true)

                    event("replace_code", Map("newCode" -> toJson(newCode)))

                    event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("success")))

                    notifySuccess("Search succeeded!")
                    logInfo("Synthesis search succeeded!")
                  } else {
                    event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("failure")))

                    notifyError("Solution was not proven.")
                    logInfo("Synthesis search failed!")
                  }
                } catch {
                  case t: Throwable =>
                    notifyError("Woops, solution processing crashed: "+t.getMessage)
                    t.printStackTrace()
                }

              case None =>
                event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("failure")))

                if (search.shouldStop) {
                  notifyError("Search cancelled")
                } else {
                  notifyError("Search failed to find a solution")
                }

            }
          case None =>
        }
      } catch {
        case t: Throwable =>
          notifyError("Woops, I crashed: "+t.getMessage())
          logInfo("Synthesis search crashed!", t)
      }


    case SynthesisApplyRule(cid, rid) =>
      try {
        val path = List(rid)
        chooses.get(cid) match {
          case Some((ci @ ChooseInfo(ctx, prog, fd, pc, ch, _), search)) =>
            search.traversePath(path) match {
              case Some(al: search.g.AndLeaf) =>
                logInfo("Applying :"+al.task.app.toString)
                val res = search.expandAndTask(al.task)
                search.onExpansion(al, res)

                val solution = res match {
                  case es: search.ExpandSuccess[_] =>
                    // Solved
                    Some(es.sol)
                  case ex: search.Expanded[_] =>
                    // Node has been updated
                    search.traversePath(path) match {
                      case Some(an: search.g.AndNode) =>
                        an.task.composeSolution(an.subTasks.map(t => Solution.choose(t.p)))
                      case _ =>
                        None
                    }
                  case _ =>
                    None
                }

                solution match {
                  case Some(sol) =>
                    val chToSol = Map(ci -> sol.toSimplifiedExpr(ctx, prog))
                    val fInt = new FileInterface(reporter)

                    val newCode = fInt.substitueChooses(codeHistory.head, chToSol, true)

                    event("replace_code", Map("newCode" -> toJson(newCode)))

                    notifySuccess("Successfully applied "+al.task.app.toString)
                    logInfo("Application successful!")

                  case None =>
                    notifyError("Failed to apply "+al.task.toString)
                    logInfo("Application failed!")

                }
              case _ =>
                notifyError("Woops, choose not found..")
                logInfo("Path "+path+" did not lead to AndLeaf!")
            }
          case _ =>
            notifyError("Woops, choose not found..")
            logInfo("Choose "+cid+" not found")
        }
      } catch {
        case t: Throwable =>
          notifyError("Woops, I crashed: "+t.getMessage())
          logInfo("Synthesis Rule Application crashed", t)
      }

    case SynthesisGetRulesToApply(chooseLine, chooseColumn) =>
      try {
        chooses.find(_._2._1.ch.posIntInfo == (chooseLine, chooseColumn)) match {
          case Some((i, (ci, search))) =>
            val orNode = search.g.tree match {
              case ol: search.g.OrLeaf =>
                val sub = search.expandOrTask(ol.task)
                search.onExpansion(ol, sub)

                // It was updated
                search.g.tree.asInstanceOf[search.g.OrNode]

              case on: search.g.OrNode =>
                on
            }

            val rulesApps = for ((t, i) <- orNode.altTasks.zipWithIndex) yield {
              val status = if (orNode.triedAlternatives contains t) {
                "closed"
              } else {
                "open"
              }

              toJson(Map("id" -> toJson(i),
                         "name" -> toJson(t.app.toString),
                         "status" -> toJson(status)))
            }

            event("synthesis_choose_rules", Map("chooseLine"   -> toJson(chooseLine),
                                                "chooseColumn" -> toJson(chooseColumn),
                                                "cid"          -> toJson(i),
                                                "rulesApps"    -> toJson(rulesApps)))

          case None =>
            notifyError("Woops, choose not found..")
            logInfo("Choose "+chooseLine+":"+chooseColumn+" not found")
        }

      } catch {
        case t: Throwable =>
          notifyError("Woops, I crashed: "+t.getMessage())
          logInfo("Synthesis RulesList crashed", t)
      }

    case msg =>
      clientLog("Unknown Actor Message: "+msg)
  }
}
