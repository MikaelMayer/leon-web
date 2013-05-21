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
import leon.termination._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.ScalaPrinter
import leon.purescala.EquivalencePrettyPrinter
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.xlang._

import java.util.concurrent.atomic.AtomicBoolean

import java.util.concurrent.TimeoutException
case class CompilationState (
  code: Option[String],
  compResult: String,
  optProgram: Option[Program],
  // Imperative information
  wasLoop: Set[FunDef],
  freshFunDefs: Map[FunDef, FunDef],
  parents: Map[FunDef, FunDef]) {

  def program: Program = {
    optProgram.get
  }

  def isCompiled = optProgram.isDefined

  def innerFunctionsOf(fd: FunDef): Set[FunDef] = {
    parents.flatMap((p: (FunDef, FunDef)) => p match {
      case (child, parent) => if(parent == fd) List(child) else List()
    }).toSet
  }

  def origFunctionFor(fd: FunDef): FunDef = {
    val originalFunDefs = freshFunDefs.map(x => (x._2, x._1))

    originalFunDefs.getOrElse(fd, fd)
  }

  def functionWasLoop(fd: FunDef): Boolean =
    wasLoop.contains(origFunctionFor(fd))

}
object CompilationState {
  def failure(code: String) =
    CompilationState(Some(code), "failure", None, Set(), Map(), Map())

  def unknown = 
    CompilationState(None, "unknown", None, Set(), Map(), Map())
}

trait BaseActor extends Actor {

  def pushMessage(v: JsValue)

  def notifySuccess(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("success")))
  }

  def logInfo(msg: String) {
    Logger.info(msg)
  }

  def logInfo(msg: String, t: Throwable) {
    Logger.info(msg, t)
  }

  def clientLog(msg: String) = {
    logInfo("[>] L: "+msg)
    pushMessage(toJson(Map("kind" -> "log", "level" -> "log", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    logInfo("[>] "+kind)
    pushMessage(toJson(Map("kind" -> toJson(kind)) ++ data))
  }


  def notifyError(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("error")))
  }

}

trait WorkerActor extends BaseActor {
  import ConsoleProtocol._

  def session: ActorRef

  def pushMessage(v: JsValue) = session ! NotifyClient(v)
}

class VerificationWorker(val session: ActorRef, doCancel: AtomicBoolean) extends Actor with WorkerActor {
  import ConsoleProtocol._

  var verifOverview  = Map[FunDef, List[VerificationCondition]]()

  def vcToJson(cstate: CompilationState, vc: VerificationCondition): JsValue = {
    def ceToJson(ce: Map[Identifier, Expr]): JsValue = {
      toJson(ce.map{ case (id, ex) =>
        id.toString -> toJson(ScalaPrinter(ex))
      })
    }

    val base = Map(
      "kind"   -> toJson(vc.kind.toString),
      "fun"    -> toJson(cstate.origFunctionFor(vc.funDef).id.name),
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

  def notifyOverview(cstate: CompilationState) {
    case class FunVerif(fd: FunDef, vcs: List[VerificationCondition]) {
      val totalTime = vcs.flatMap(_.time).foldLeft(0d)(_ + _)
      var status = getOverallVCsStatus(vcs)
    }

    var verifResults = verifOverview.map { case (fd, vcs) =>
      (fd, FunVerif(fd, vcs))
    }

    if (cstate.isCompiled) {
      for ((fd, fv) <- verifResults if fv.status != "valid") {
        for (cfd <- cstate.program.transitiveCallers(fd) if verifResults.get(cfd).map(_.status) == Some("valid")) {
          verifResults(cfd).status = "cond-valid"
        }
      }
    }

    val fvcs = toJson(verifResults.toSeq.sortWith{ (a,b) => a._1 < b._1 }.map{ case (fd, fv) =>
      val v = toJson(Map(
        "status" -> toJson(fv.status),
        "time" -> toJson(fv.totalTime),
        "vcs"  -> toJson(fv.vcs.map(vcToJson(cstate, _)))
      ))

      cstate.origFunctionFor(fv.fd).id.name -> v
    }.toMap)

    event("update_overview", Map("module" -> toJson("verification"), "overview" -> fvcs))
  }

  def doVerify(cstate: CompilationState, funs: Set[FunDef], standalone: Boolean) {
    val verifTimeout = 3000L // 3sec

    val reporter = new SilentReporter
    var compContext  = leon.Main.processOptions(reporter, List("--feelinglucky", "--evalground"))

    val solvers = List(
      new TimeoutSolver(new TrivialSolver(compContext), verifTimeout),
      new TimeoutSolver(new FairZ3Solver(compContext), verifTimeout)
    )

    solvers.map(_.setProgram(cstate.program))

    val vctx = VerificationContext(compContext, solvers, reporter)

    val vcs = verifOverview.collect {
      case (fd, vcs) if funs(fd) => fd -> vcs
    }

    try {
      val vr = AnalysisPhase.checkVerificationConditions(vctx, vcs)

      val report = XlangAnalysisPhase.completeVerificationReport(vr, cstate.parents, cstate.functionWasLoop _)

      val status = getOverallVCsStatus(report.conditions)

      for ((f, vcs) <- report.fvcs) {
        verifOverview += f -> vcs
      }
    } catch {
      case t: Throwable =>
        logInfo("[!] Verification crashed!", t)
    }

    notifyOverview(cstate)
  }

  def receive = {
    case OnUpdateCode(cstate) if cstate.isCompiled =>
      val program = cstate.program
      val reporter = new SilentReporter

      var toGenerate = Set[FunDef]()
      val oldVerifOverView = verifOverview

      val verifFunctions = program.definedFunctions.filter(fd => fd.hasBody).toSet

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

      verifOverview = verifOverview.filterKeys(verifFunctions)

      val toInvalidate = toGenerate.flatMap { program.transitiveCallers _ }

      toGenerate ++= toInvalidate

      if (!toGenerate.isEmpty) {
        clientLog("Generating VCs...")

        toGenerate.foreach{ fd =>
          toGenerate ++= cstate.innerFunctionsOf(fd)
        }

        // Generate VCs
        val fvcs = AnalysisPhase.generateVerificationConditions(reporter, program, toGenerate.map(_.id.name))

        for ((f, vcs) <- fvcs) {
          verifOverview += f -> vcs
        }

        clientLog("Done!")

        notifyOverview(cstate)
      }

      doVerify(cstate, toGenerate, false)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case "doVerify" =>
          val fname = (event \ "fname").as[String]

          val realfname = cstate.freshFunDefs.find(_._1.id.name == fname) match {
            case Some((fd1, fd2)) => fd2.id.name
            case None => fname
          }

          verifOverview.keySet.find(_.id.name == realfname) match {
            case Some(fd) =>
              doVerify(cstate, Set(fd) ++ cstate.innerFunctionsOf(fd), true)
            case None =>
              logInfo("Function "+fname+" ~ "+realfname+" not found!")
          }

        case action =>
          notifyError("Received unknown action: "+action)
      }
  }
}

class TerminationWorker(val session: ActorRef, doCancel: AtomicBoolean) extends Actor with WorkerActor {
  import ConsoleProtocol._

  def notifyOverview(cstate: CompilationState, data: Map[FunDef, TerminationGuarantee]) {
    if (cstate.isCompiled) {
      val facts = for ((fd, tg) <- data) yield {
        val t = toJson(Map(
          "status" -> toJson(if (tg.isGuaranteed) "terminates" else "noguarantee")
        ))

        cstate.origFunctionFor(fd).id.name -> t
      }

      event("update_overview", Map("module" -> toJson("termination"), "overview" -> toJson(facts.toMap)))
    }

  }

  def receive = {
    case OnUpdateCode(cstate) if cstate.isCompiled =>

      val reporter = new SilentReporter
      var ctx      = leon.Main.processOptions(reporter, List())

      val tc = new SimpleTerminationChecker(ctx, cstate.program)

      val data = (cstate.program.definedFunctions.toList.sortWith(_ < _).map { funDef =>
        (funDef -> tc.terminates(funDef))
      }).toMap

      notifyOverview(cstate, data)

    case _ =>
  }
}

class SynthesisWorker(val session: ActorRef, doCancel: AtomicBoolean) extends Actor with WorkerActor {
  import ConsoleProtocol._

  var choosesInfo = Map[String, Seq[(ChooseInfo, SimpleWebSearch)]]()

  def notifyOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      val facts = for ((fname, sps) <- choosesInfo) yield {
        val problems = for (((ci, search), i) <- sps.zipWithIndex) yield {
          Map(
            "description" -> toJson(ci.problem.toString),
            "problem" -> toJson(ci.problem.toString),
            "line" -> toJson(ci.ch.posIntInfo._1),
            "column" -> toJson(ci.ch.posIntInfo._2),
            "index" -> toJson(i)
          )
        }
        fname -> toJson(problems)
      }

      event("update_synthesis_overview", Map("functions" -> toJson(facts)))
    }

  }

  def receive = {
    case OnUpdateCode(cstate) =>
      var options = SynthesisOptions().copy(cegisGenerateFunCalls = true)
      var context = leon.Main.processOptions(new SilentReporter, Nil)

      choosesInfo = ChooseInfo.extractFromProgram(context, cstate.program, options).map {
        case ci =>
          val search = new SimpleWebSearch(this, ci.synthesizer, ci.problem)
          (ci, search)
      }.groupBy(_._1.fd.id.name)

      notifyOverview(cstate)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case "getRulesToApply" =>
          val chooseLine   = (event \ "chooseLine").as[Int]
          val chooseColumn = (event \ "chooseColumn").as[Int]

          self ! SynthesisGetRulesToApply(chooseLine, chooseColumn)

        case "doApplyRule" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          val ruleId = (event \ "rid").as[Int]

          doApplyRule(cstate, fname, chooseId, ruleId)

        case "doSearch" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]

          doSearch(cstate, fname, chooseId)
      }
  }

  def doApplyRule(cstate: CompilationState, fname: String, cid: Int, rid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci, search)) =>

      case None =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }

  def doSearch(cstate: CompilationState, fname: String, cid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, _, ch, _), search)) =>
        try {
          ci.synthesizer.shouldStop.set(false)

          search.search() match {
            case Some((sol, isTrusted)) =>
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

                event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("success")))

                notifySuccess("Search succeeded!")
                logInfo("Synthesis search succeeded!")
              } else {
                event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("failure")))

                notifyError("Solution was not proven.")
                logInfo("Synthesis search failed!")
              }
            case None =>
              event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("failure")))

              notifyError("Search failed.")
              logInfo("Synthesis search failed!")
          }
        } catch {
          case t: Throwable =>
            notifyError("Woops, search crashed: "+t.getMessage)
            logInfo("Synthesis search crashed", t)
        }
      case None =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }

    /*
    case SynthesisSearch(cid) =>
      assumeCompiled { cstate =>
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

                      val newCode = fInt.substitueChooses(cstate.code.getOrElse(""), chToSol, true)

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
      }


    case SynthesisApplyRule(cid, rid) =>
      assumeCompiled { cstate =>
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

                      val newCode = fInt.substitueChooses(cstate.code.getOrElse(""), chToSol, true)

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
      */
}

class ConsoleSession(remoteIP: String) extends Actor with BaseActor {
  import context.dispatcher
  import ConsoleProtocol._

  var channel: PushEnumerator[JsValue] = _
  var reporter: WSReporter = _

  def pushMessage(v: JsValue) = channel.push(v)

  var lastCompilationState: CompilationState = CompilationState.unknown

  def assumeCompiled[A](f: CompilationState => A) = {
    lastCompilationState match {
      case cstate if cstate.isCompiled =>
        f(cstate)
      case _ =>
        notifyError("Not compiled ?!")
        logInfo("Not compiled ?!")
    }
  }

  var chooses: Map[Int, (ChooseInfo, SimpleSearch)] = Map()



  def resetContext() {
    chooses = Map()
  }

  case class ModuleContext(name: String, actor: ActorRef, cancelFlag: AtomicBoolean)

  var modules = Map[String, ModuleContext]()

  def receive = {
    case Init =>
      channel  = Enumerator.imperative[JsValue]()
      reporter = new WSReporter(channel)
      sender ! InitSuccess(channel)

      val cancelFlag = new AtomicBoolean()

      modules += "verification" -> ModuleContext("verification", Akka.system.actorOf(Props(new VerificationWorker(self, cancelFlag))), cancelFlag)
      modules += "termination"  -> ModuleContext("termination",  Akka.system.actorOf(Props(new TerminationWorker(self, cancelFlag))), cancelFlag)
      modules += "synthesis"    -> ModuleContext("termination",  Akka.system.actorOf(Props(new SynthesisWorker(self, cancelFlag))), cancelFlag)

      logInfo("New client")

    case NotifyClient(event) =>
      pushMessage(event)

    case ProcessClientEvent(event) =>
      try {
        logInfo("[<] "+(event \ "action").as[String])

        (event \ "module").as[String] match {
          case "main" =>
            (event \ "action").as[String] match {
              case "doUpdateCode" =>
                self ! UpdateCode((event \ "code").as[String])

              case "storePermaLink" =>
                self ! StorePermaLink((event \ "code").as[String])

              case "accessPermaLink" =>
                self ! AccessPermaLink((event \ "link").as[String])
            }

          case m if modules contains m =>
            modules(m).actor ! ProcessClientEvent(event)

          case m =>
            notifyError("Module "+m+" not available.")

        }
      } catch {
        case t: Throwable =>
          notifyError("Could not process event: "+t.getMessage)
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
      if (lastCompilationState.code != code) {
        clientLog("Compiling...")
        logInfo("Code to compile:\n"+code)

        val compReporter = new CompilingWSReporter(channel)
        var compContext  = leon.Main.processOptions(compReporter, Nil)
        //var synthContext = compContext.copy(reporter = reporter)

        val pipeline = TemporaryInputPhase andThen ExtractionPhase

        resetContext()

        try {
          val pgm = pipeline.run(compContext)((code, Nil))

          val pgm1 = ArrayTransformation(compContext, pgm)
          val pgm2 = EpsilonElimination(compContext, pgm1)
          val (pgm3, wasLoop) = ImperativeCodeElimination.run(compContext)(pgm2)
          val (program, parents, freshFunDefs) = FunctionClosure.run(compContext)(pgm3)

          val cstate = CompilationState(
            optProgram = Some(program),
            code = Some(code),
            compResult = "success",
            wasLoop = wasLoop,
            freshFunDefs = freshFunDefs,
            parents = parents
          )

          lastCompilationState = cstate

          // Extract Synthesis Problems
          println(ScalaPrinter(program))
          //var options = SynthesisOptions()
          //options = options.copy(cegisGenerateFunCalls = true)
          //chooses = ChooseInfo.extractFromProgram(synthContext, program, options).zipWithIndex.map {
          //  case (ci, i) =>
          //    val search = new SimpleWebSearch(this, ci.synthesizer, ci.problem)
          //    (i+1) -> (ci, search)
          //}.toMap

          // Extract Verification Problems

          event("compilation", Map("status" -> toJson("success")))

          clientLog("Compilation successful!")

          notifyOverview(cstate)

          notifyAnnotations(Seq())

          modules.values.foreach (_.actor ! OnUpdateCode(cstate))

        } catch {
          case t: Throwable =>
            chooses = Map()

            logInfo("Compilation failed: "+t.getMessage)
            for ((l,e) <- compReporter.errors) {
              logInfo("  "+e.mkString("\n  "))
            }
            clientLog("Compilation failed!")

            event("compilation", Map("status" -> toJson("failure")))

            val annotations = compReporter.errors.map{ case (l,e) =>
              CodeAnnotation(l, 0, e.mkString("\n"), CodeAnnotationError)
            }.toSeq

            notifyAnnotations(annotations)

            lastCompilationState = CompilationState.failure(code)


        }
      } else {
        event("compilation", Map("status" -> toJson(lastCompilationState.compResult)))
      }

    case Quit =>


    case msg =>
      clientLog("Unknown Actor Message: "+msg)
  }

  def notifyOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      val facts = for (fd <- cstate.program.definedFunctions.toList.sortWith(_ < _)) yield {
        toJson(Map(
          "name" -> toJson(fd.id.name),
          "line" -> toJson(fd.posIntInfo._1),
          "column" -> toJson(fd.posIntInfo._2)
        ))
      }

      event("update_overview", Map("module" -> toJson("main"), "overview" -> toJson(facts)))
    }

  }

  def notifyAnnotations(annotations: Seq[CodeAnnotation]) {
    event("editor", Map("annotations" -> toJson(annotations.map(_.toJson))))
  }
}

