package leon

import japgolly.scalajs.react.React
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import dom.html.Element
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.collection.mutable.{HashMap => MMap}
import js.annotation._
import com.scalawarrior.scalajs.ace._
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}

import js.Dynamic.{global => g, literal => l}

@ScalaJSDefined
class ExplorationFact(val range: Range, val res: String) extends js.Object

object ExplorationFact {
  def apply(range: Range, res: String): ExplorationFact = new ExplorationFact(range, res)
}

class Feature(_a: Boolean, _n: String) {
  @JSExport var active: Boolean = _a
  @JSExport val name: String = _n
}
object Feature { def apply(active: Boolean, name: String) = new Feature(active, name).asInstanceOf[js.Any] }


@JSExport("Main")
object Main extends js.JSApp {
  import Bool._
  import JQueryExtended._
  import js.JSON
  import dom.alert
  def window = g

  @JSExport
  def main(): Unit = {
    println("Application starting")
  }
  
  @ScalaJSDefined
  trait LocalStorage extends js.Any {
    def getItem(name: String): String
    def setItem(name: String, value: String): Unit
  }
  
  def localStorage = window.localStorage.asInstanceOf[LocalStorage]
  
  var editor = ace.edit("codebox");
  var aceRange = ace.require("ace/range").Range;
  ace.require("ace/token_tooltip");
  editor.setTheme("ace/theme/chrome");
  editor.getSession().setMode("ace/mode/scala")
  editor.getSession().setUseWrapMode(true)
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView();
  editor.setHighlightActiveLine(false);
  editor.getSession().setTabSize(2)

  var hash = window.location.hash

  var WS = js.isUndefined(g.MozWebSocket) ? g.MozWebSocket | g.WebSocket
  var leonSocket: js.Dynamic = null

  var headerHeight = $("#title").height()+20

  var lastRange: Range = null;
  var lastDisplayedRange: Range = null;
  var lastProcessedRange: Range = null;

  var explorationFacts = new js.Array[ExplorationFact]();

  var displayedMarker = -1;
  
  def clearExplorationFacts() = {
    lastRange = null;
    lastProcessedRange = null;

    hideHighlight();

    explorationFacts = new js.Array[ExplorationFact]();
  }
  
  def hideHighlight() = {
    if (displayedMarker > 0) {
      editor.getSession().removeMarker(displayedMarker);

      $(".leon-explore-location.ace_start").each((index: js.Any, _this: dom.Element) => 
        $(_this).tooltip("destroy").asInstanceOf[js.Any]
      );

    }

    lastDisplayedRange = null;
    displayedMarker = -1
  }
  
  @ScalaJSDefined
  trait NewResult extends js.Object {
    val fromRow: Int
    val fromColumn: Int
    val toRow: Int
    val toColumn: Int
    val result: String
  }

  def updateExplorationFacts(newResults: js.Array[NewResult]) {
    for (i <- 0 until newResults.length) {
      var n = newResults(i);

      explorationFacts.push(ExplorationFact(
        range = js.Dynamic.newInstance(aceRange)(n.fromRow, n.fromColumn, n.toRow, n.toColumn).asInstanceOf[Range],
        res = n.result
      ));
    }

    displayExplorationFacts()
  }
  
  def showHighlight(range: Range, content: String) = {
    if (range != lastDisplayedRange) {
      hideHighlight()

      lastDisplayedRange = range;

      displayedMarker = editor.getSession().addMarker(range, "leon-explore-location", "text", true);

      js.timers.setTimeout(50){
        $(".leon-explore-location.ace_start").tooltip(l(
            title = content,
            container = "#codebox",
            placement = "top",
            trigger = "manual"
        ))
        $(".leon-explore-location.ace_start").tooltip("show");
      }
    }
  }

  

  editor.getSession().on("changeScrollTop", (_ :js.Any) => {
      hideHighlight();
  });

  def rangeScore(start: Position, end: Position): Double = {
      if (start.row == end.row) {
          return (end.row - start.row)*80 + end.column - start.column;
      } else {
          return (end.row - start.row)*80 + end.column - start.column;
      }
  }
  
  val features = l(
    verification=   Feature(active= true, name= "Verification"),
    synthesis=      Feature(active= true, name= "Synthesis"),
    termination=    Feature(active= false, name= "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>"),
    presentation=   Feature(active= false, name= "Presentation Mode"),
    execution=      Feature(active= true, name= "Execution"),
    repair=         Feature(active= true, name= "Repair <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
  )

  def displayExplorationFacts() = {
      if (features.execution.active && explorationFacts.length > 0) {
          var lastRange = editor.selection.getRange();

          if (js.isUndefined(lastProcessedRange) || !lastRange.isEqual(lastProcessedRange)) {
              var maxScore = 0.0
              var maxRes: ExplorationFact = null

              for(r <- explorationFacts) {
                  var score = 0.0;

                  var cmp = lastRange.compareRange(r.range)

                  var found = ((cmp >= -1) && (cmp <= 1));

                  if (cmp == -1) {
                      var match_s = lastRange.start
                      var match_e = r.range.end
                      var before_s = r.range.start
                      var after_e = lastRange.end

                      score = rangeScore(match_s, match_e) -
                              rangeScore(before_s, match_s) -
                              rangeScore(match_e, after_e);

                  } else if (cmp == 0) {
                      if (lastRange.containsRange(r.range)) {
                          var match_s = r.range.start
                          var match_e = r.range.end
                          var before_s = lastRange.start
                          var after_e = lastRange.end

                          score = rangeScore(match_s, match_e) -
                                  rangeScore(before_s, match_s) -
                                  rangeScore(match_e, after_e);
                      } else {
                          var match_s = lastRange.start
                          var match_e = lastRange.end
                          var before_s = r.range.start
                          var after_e = r.range.end

                          score = rangeScore(match_s, match_e) -
                                  rangeScore(before_s, match_s) -
                                  rangeScore(match_e, after_e);
                      }
                  } else if (cmp == 1) {
                      var match_s = r.range.start
                      var match_e = lastRange.end
                      var before_s = lastRange.start
                      var after_e = r.range.end

                      score = rangeScore(match_s, match_e) -
                              rangeScore(before_s, match_s) -
                              rangeScore(match_e, after_e);
                  }

                  if (found && (maxRes == null || maxScore < score)) {
                      maxScore = score
                      maxRes = r
                  }
              }

              if (maxRes != null) {
                  showHighlight(maxRes.range, maxRes.res)
              } else {
                  hideHighlight();
              }
          }

          lastProcessedRange = lastRange
      }
  }

  $("#codecolumn").mouseup((e: JQueryEventObject) => {
      displayExplorationFacts();
  }.asInstanceOf[js.Any])

  $("#codecolumn").keyup((e: JQueryEventObject) => {
      displayExplorationFacts();
  }.asInstanceOf[js.Any])

  $(".menu-button").click(((self: Element, event: JQueryEventObject) => {
      var target = $(self).attr("ref")
      var sel = "#"+target

      if ($(sel).is(":visible")) {
          $(sel).hide()
          $(self).addClass("disabled")
      } else {
          $(sel).show()
          $(self).removeClass("disabled")
      }

  }): js.ThisFunction);

  $("#button-save").click((event: JQueryEventObject) => {
      recompile()
      event.preventDefault()
  });

  $("#button-undo").click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
          doUndo()
      }
      event.preventDefault()
  }): js.ThisFunction);

  $("#button-redo").click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
          doRedo()
      }
      event.preventDefault()
  }): js.ThisFunction);

  def hasLocalStorage(): Boolean = {
    try {
      return !js.isUndefined(window.localStorage) && window.localStorage != null;
    } catch {
      case e: Exception =>
      return false;
    }
  }

  var handlers = js.Dictionary.empty[Any]
  var compilationStatus = 0
  var searchFinished = false
  var context = "unknown";

  var maxHistory = 20;
  // Undo/Redo
  var backwardChanges = JSON.parse(localStorage.getItem("backwardChanges")).asInstanceOf[js.Array[String]]
  if (js.isUndefined(backwardChanges)) {
    backwardChanges = new js.Array[String]()
  }
  var forwardChanges  = JSON.parse(localStorage.getItem("forwardChanges").asInstanceOf[String]).asInstanceOf[js.Array[String]]
  if (js.isUndefined(forwardChanges)) {
    forwardChanges = new js.Array[String]()
  }

  def doUndo() {
    forwardChanges.push(editor.getValue());
    var code = backwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  def doRedo() {
    backwardChanges.push(editor.getValue());
    var code = forwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  def storeCurrent(code: String) {
    forwardChanges = new js.Array[String]()
    if (backwardChanges.length >= 1) {
      if (code != backwardChanges(backwardChanges.length-1)) {
        backwardChanges.push(code)
      }
    } else {
        backwardChanges.push(code)
    }
    updateUndoRedo()
  }

  def updateUndoRedo() {
    var ub = $("#button-undo") 
    var rb = $("#button-redo") 

    if (backwardChanges.length > 0) {
      ub.removeClass("disabled") 
    } else {
      ub.addClass("disabled") 
    }

    if (forwardChanges.length > 0) {
      rb.removeClass("disabled") 
    } else {
      rb.addClass("disabled") 
    }

    if (backwardChanges.length > maxHistory) {
      backwardChanges.splice(0, backwardChanges.length-maxHistory)
    }

    if (forwardChanges.length > maxHistory) {
      forwardChanges.splice(0, forwardChanges.length-maxHistory)
    }

    localStorage.setItem("backwardChanges", JSON.stringify(backwardChanges));
    localStorage.setItem("forwardChanges",  JSON.stringify(forwardChanges));
  }

  updateUndoRedo()

  $("#button-permalink").click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
          var msg = JSON.stringify(
            l(action= "storePermaLink", module= "main", code= editor.getValue())
          )
          leonSocket.send(msg)
      }
      event.preventDefault()
  }): js.ThisFunction);

  handlers("permalink") = (data: js.Dynamic) => {
      $("#permalink-value input").value(window._leon_url+"#link/"+data.link)
      $("#permalink-value").show()
  }

  $("#button-permalink-close").click((event: JQueryEventObject) => {
      $("#permalink-value").hide()
  })

  /**
   * Compilation
   */

  def updateCompilationProgress(percents: Int) {
    $("#overview .progress-bar").css("width", percents+"%");
  }

  def updateCompilationStatus(status: String) {
      var e = $(".compilation-status")
      var codebox = $("div#codebox")
      var boxes = $(".results_table")

      e.attr("class", "compilation-status");
      $(".results_table > .overlay").remove();

      if (status == "success") {
        compilationStatus = 1

        e.addClass("success")
        e.html("""Compiled <i class="fa fa-check" title="Compilation succeeded"></i>""")


        codebox.removeClass("compilation-error")
      } else if (status == "failure") {
        compilationStatus = -1

        e.addClass("failure")
        e.html("""Compilation Failed <i class="fa fa-warning" title="Compilation failed"></i>""")

        codebox.addClass("compilation-error")

        boxes.append("""<div class="overlay" />""")
      } else if (status == "disconnected") {
        compilationStatus = 0

        e.addClass("failure")
        e.html("""Disconnected <i class="fa fa-unlink"></i>""")

        boxes.append("""<div class="overlay" />""")

      } else if (status == "unknown") {
        compilationStatus = 0

        e.html("""Compiling <i class="fa fa-refresh fa-spin" title="Compiling..."></i>""")
      } else {
        alert("Unknown status: "+status)
      }

      if (status == "unknown") {
        updateCompilationProgress(0);
      } else {
        updateCompilationProgress(100);
      }

      clearExplorationFacts();
      drawSynthesisOverview()
  }
  handlers("compilation_progress") = (data: { val total: Float; val current: Float } ) => {
    updateCompilationProgress(Math.round((data.current*100)/data.total))
  }

  handlers("compilation") = (data: { val status: String}) => {
      if(data.status == "success") {
          updateCompilationStatus("success")
      } else {
          updateCompilationStatus("failure")
      }
  }

  handlers("move_cursor") = (data: { val line: Double }) => {
    editor.selection.clearSelection();
    editor.gotoLine(data.line);
  }

  

  var localFeatures = localStorage.getItem("leonFeatures")
  if (localFeatures != null) {
    var locFeatures = JSON.parse(localFeatures) //TODO: Better serialization
    for (f <- js.Object.keys(locFeatures.asInstanceOf[js.Object])) {
      if (!js.isUndefined(features(f))) {
          features.asInstanceOf[js.Dictionary[Feature]](f).active = locFeatures(f).active
      }
    }
  }

  var fts = $("#params-panel ul")
  for (f <- js.Object.keys(features.asInstanceOf[js.Object])) {
      fts.append("""<li><label class="checkbox"><input id="feature-""""+f+" class=\"feature\" ref=\""+f+"\" type=\"checkbox\""+(features.asInstanceOf[js.Dictionary[Feature]](f).active ? """ checked="checked"""" | "")+">"+features.asInstanceOf[js.Dictionary[Feature]](f).name+"</label></li>")
  }

  $(".feature").click(((self: Element) => {
      var f = $(self).attr("ref")
      features.asInstanceOf[js.Dictionary[Feature]](f).active = !features.asInstanceOf[js.Dictionary[Feature]](f).active

      var msg = JSON.stringify(
        l(action= "featureSet", module= "main", feature= f, active= features.asInstanceOf[js.Dictionary[Feature]](f).active)
      )
      leonSocket.send(msg)


      localStorage.setItem("leonFeatures", JSON.stringify(features.asInstanceOf[js.Dictionary[Feature]]));

      recompile()

      drawOverView()
      drawSynthesisOverview()
      setPresentationMode()
  }): js.ThisFunction)

  setPresentationMode()

  @ScalaJSDefined
  trait Overview extends js.Object {
    val modules: js.Dynamic
    val data: js.Dynamic
    val functions: js.Dynamic
  }
  
  trait D {
    val status: String
    val vcs: js.Array[Any]
  }
  
  val overview: Overview = new Overview {
      val modules= l(
          verification= l(
              column= "Verif.",
              html= (name: String, d: D) => {
                  val vstatus = d.status match {
                    case "crashed" =>
                      """<i class="fa fa-bolt text-danger" title="Unnexpected error during verification"></i>"""
                    case "undefined" =>
                      """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
                    case "cond-valid" =>
                      """<span class="text-success" title="Conditionally valid">(<i class="fa fa-check"></i>)</span>"""
                    case "valid" =>
                      """<i class="fa fa-check text-success" title="Valid"></i>"""
                    case "invalid" =>
                      """<i class="fa fa-exclamation-circle text-danger" title="Invalid"></i>""";
                    case "timeout" =>
                      """<i class="fa fa-clock-o text-warning" title="Timeout"></i>"""
                    case _ =>
                      """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
                  }

                  "<td class=\"status verif\" fname=\""+name+"\">"+vstatus+"</td>"
              },
              missing= (name: String) => {
                  "<td class=\"status verif\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
              },
              handlers= () => {
                  $("td.verif").click(((self: Element) => {
                      var fname = $(self).attr("fname")
                      if (!js.isUndefined(overview.data("verification")(fname))) {
                          var d = overview.data("verification")(fname).asInstanceOf[D]

                          openVerifyDialog()

                          displayVerificationDetails(d.status, d.vcs)
                      } else {
                          openVerifyDialog()

                          displayVerificationDetails("unknown", new js.Array[Any]())
                      }
                  }): js.ThisFunction)
              }
          ),
          termination= l(
              column= "Term.",
              html= (name: String, d: D) => {
                  val tstatus = d.status match {
                      case "wip" =>
                          """<i class="fa fa-refresh fa-spin" title="Checking termination..."></i>""";
                      case "terminates" =>
                          """<i class="fa fa-check text-success" title="Termination guaranteed"></i>""";
                      case "loopsfor" =>
                          """<i class="fa fa-exclamation-circle text-danger" title="Non-terminating"></i>""";
                      case "callsnonterminating" =>
                          """<span class="text-success" title="Calls non-terminating functions">(<i class="fa fa-check text-success"></i>)</span>""";
                      case "noguarantee" =>
                          """<i class="fa fa-question text-danger" title="No termination guarantee"></i>""";
                      case _ =>
                          """<i class="fa fa-question" title="Unknown" />""";
                  }

                  "<td class=\"status termin\" fname=\""+name+"\">"+tstatus+"</td>"
              },
              missing= (name: String) => {
                  "<td class=\"status termin\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
              },
              handlers= () => {
                  $("td.termin").click(((self: Element) => {
                      var fname = $(self).attr("fname")
                      openTerminationDialog()
                      if (!js.isUndefined(overview.data("termination")(fname))) {
                          var d = overview.data("termination")(fname).asInstanceOf[D]
                          displayTerminationDetails(d.status, d)
                      } else {
                          displayTerminationDetails("unknown", null)
                      }
                  }): js.ThisFunction);
              }
          )
      )
      val functions= l()
      val data= l(
          verification= l(),
          termination= l()
      )
  }
  println("until here now !!!")
  def recompile() = {} //Mock
  def displayVerificationDetails(status: String, vcs: js.Array[Any]) {}  //Mock
  def drawSynthesisOverview() {}  //Mock
  def drawOverView() {} // Mock
  def setPresentationMode() {} // Mock
  def openVerifyDialog() {} // Mock
  def openTerminationDialog() {} // Mock
  def displayTerminationDetails(s: String, d: D) {} // Mock
  /*

  handlers("update_overview") = function(data) {
      if (data.module == "main") {
          overview.functions = {};

          for (var i = 0; i < data.overview.length; i++) {
              var fdata = data.overview[i]
              var fname = fdata.name
              overview.functions[fname] = fdata;
          }
      } else {
          overview.data[data.module] = data.overview
      }

      drawOverView()
  }

  var synthesisOverview = {}

  handlers("update_synthesis_overview") = function(data) {
      if (JSON.stringify(synthesisOverview) != JSON.stringify(data)) {
          synthesisOverview = data;
          drawSynthesisOverview();
      }
  }


  function drawSynthesisOverview() {
      var t = $("#synthesis_table")
      var html = "";

      function addMenu(index, fname, description) {
          var id = """menu"""+fname+index

          html += """ <div class="dropdown">"""
          html += """  <a id=""""+id+"""" href="#" role="button" class="dropdown-toggle" data-toggle="dropdown"> <i class="fa fa-magic"></i> """+description+"""</a>"""
          html += """  <ul class="dropdown-menu" role="menu" aria-labelledby=""""+id+"""">"""
          if (compilationStatus == 1) {
              html += """    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="search" cid=""""+index+"""">Search</a></li>"""
              html += """    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="explore" cid=""""+index+"""">Explore</a></li>"""
              html += """    <li role="presentation" class="divider"></li>"""
              html += """    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><img src="/assets/images/loader.gif" /></a></li>"""
          } else {
              html += """    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><i class="fa fa-ban"></i> Not compiled</a></li>"""
          }

          html += """  </ul>"""
          html += """ </div>"""
      }

      var data = synthesisOverview

      var fnames = []
      for (var f in data.functions) {
        fnames.push(f)
      }
      fnames.sort()

      for (var fi = 0; fi < fnames.length; fi++) {
          var  f = fnames[fi];
          if (f in overview.functions) {
              if (data.functions[f].length == 1) {
                  var sp = data.functions[f][0]
                  html += "<tr><td class=\"fname problem  clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
                  addMenu(sp.index, f, overview.functions[f].displayName)
                  html += "</td></tr>"
              } else {
                  html += "<tr><td class=\"fname clicktoline\" line=\""+overview.functions[f].line+"\">"+overview.functions[f].displayName+"</td></tr>"
                  for (var i = 0; i < data.functions[f].length; i++) {
                      var sp = data.functions[f][i]
                      html += "<tr>"
                      html += "<td class=\"problem subproblem clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
                      addMenu(sp.index, f, sp.description)
                      html += "</td></tr>"
                  }
              }
          }
      }

      t.html(html);

      if (compilationStatus == 1) {
          $("#synthesis .dropdown-toggle").click(function(e) {
              var p = $(this).parents(".problem")

              var msg = JSON.stringify({
                  module: "synthesis",
                  action: "getRulesToApply",
                  fname: p.attr("fname"),
                  cid: 1*p.attr("cid"),
              })

              leonSocket.send(msg)
          })
      }

      if (data.functions && Object.keys(data.functions).length > 0 && features["synthesis"].active) {
          $("#synthesis").show()
      } else {
          $("#synthesis").hide()
      }
  }

  function setPresentationMode() {
      if(features["presentation"].active) {
          $("body").addClass("presentation")
      } else {
          $("body").removeClass("presentation")
      }
      resizeEditor()
  }

  handlers("update_exploration_facts") = (data: js.Dynamic) => {
      updateExplorationFacts(data.newFacts);
  }

  function drawOverView() {
      var t = $("#overview_table")
      var html = "";

      html += "<tr>"
      html += "<th>Function</th>"
      for (var m in overview.modules) {
          if (features[m].active) {
              html += "<th>"+overview.modules[m].column+"</th>"
          }
      }
      html += "</tr>"

      for (var fname in overview.functions) {
          var fdata = overview.functions[fname]

          html += "<tr>"
          html += "  <td class=\"fname clicktoline\" line=\""+fdata.line+"\">"+fdata.displayName+"</td>"
          for (var m in overview.modules) {
              if (features[m].active) {
                  var mod = overview.modules[m]
                  var data = overview.data[m]
                  if (fname in data) {
                      html += mod.html(fname, data[fname])
                  } else {
                      html += mod.missing(fname)
                  }
              }
          }
          html += "</tr>"
      }

      t.html(html);

      for (var m in overview.modules) {
          if ("handlers" in overview.modules[m]) {
              overview.modules[m].handlers()
          }
      }


      addClickToLine("#overview_table");
      addHoverToLine("#overview_table");

      if (Object.keys(overview.functions).length == 0) {
          t.hide()
      } else {
          t.show()
      }
  }

  function addClickToLine(within) {
    $(within+" .clicktoline[line]").click(function() {
        var line = $(this).attr("line")
        editor.gotoLine(line);
    })
  }

  function addHoverToLine(within) {
    $(within+" .hovertoline[line]").hover(function() {
        var line = $(this).attr("line")
        editor.gotoLine(line);
    }, function() {})
  }

  handlers("editor") = function (data) {
      if ("annotations" in data) {
          var session = editor.getSession();

          context = "unknown";

          $("#annotations").html("");

          for (var i = 0; i < data.annotations.length; i++) {
              var a = data.annotations[i];
              if (a.type == "verification") {
                  context = "verification";
              } else if (a.type == "synthesis") {
                  context = "synthesis";
              }

              if (a.type != "info" && a.type != "error") {
                  session.addGutterDecoration(a.row, "leon_gutter_"+a.type)
                  a.type = "info";
              }

              if (a.type == "error") {
                var line = a.row+1
                $("#annotations").append("<li class=\"clicktoline\" line=\""+line+"\"><code><i class=\"fa fa-warning\"></i> "+line+":"+a.text+"</code></li>")
              }
          }


          addClickToLine("#annotations");
          session.setAnnotations(data.annotations);
          resizeEditor();
      }
  }

  handlers("notification") = function (data) {
      notify(data.content, data.type);
  }

  handlers("log") = function (data) {
      var txt = $("#console")
      txt.append(data.message+"\n");
      txt.scrollTop(txt[0].scrollHeight - txt.height())
  }

  var synthesizing = false;

  handlers("synthesis_result") = function(data) {
      var pb = $("#synthesisProgress")
      var pbb = $("#synthesisProgress .progress-bar")

      // setup and open pane
      if (data.result == "init") {
          $("#synthesisResults").hide()
          $("#synthesisDialog").attr("cid", data.cid)
          $("#synthesisDialog").attr("fname", data.fname)
          $("#synthesisDialog .exploreButton").hide()
          $("#synthesisDialog .importButton").hide()
          $("#synthesisDialog .closeButton").hide()
          $("#synthesisDialog .cancelButton").show()
          $("#synthesisDialog .code.problem").removeClass("prettyprinted")
          $("#synthesisDialog .code.problem").html(data.problem)
          prettyPrint();
          $("#synthesisDialog").modal("show")

          pbb.addClass("active progress-bar-striped")
          pbb.removeClass("progress-bar-success progress-bar-danger")
          pbb.width("100%")
          pbb.html("Synthesizing...");

          $("#synthesisProgressBox").show()
          synthesizing = true;
          $("#synthesisDialog").unbind("hide.bs.modal").on("hide.bs.modal", function () {
              if (synthesizing) {
                  var msg = JSON.stringify({
                      module: "main",
                      action: "doCancel"
                  })

                  leonSocket.send(msg)
              }
          })
      } else if (data.result == "progress") {
          var pc = (data.closed*100)/data.total;
          pbb.width(pc+"%")
          pbb.html(data.closed+"/"+data.total);

      } else if (data.result == "failure") {
          pbb.removeClass("active progress-bar-striped")

          pbb.width("100%")
          pbb.html("Failed to apply");
          pbb.addClass("progress-bar-danger")

          $("#synthesisDialog .importButton").hide()
          $("#synthesisDialog .exploreButton").hide()
          $("#synthesisDialog .cancelButton").hide()
          $("#synthesisDialog .closeButton").show()
          synthesizing = false;

      } else if (data.result == "success") {
          pbb.removeClass("active progress-bar-striped")

          pbb.width("100%")
          pbb.html(data.closed+"/"+data.total);
          pbb.addClass("progress-bar-success")

          $("#synthesisResults .code.solution").removeClass("prettyprinted")
          $("#synthesisResults .code.solution").html(data.solCode)
          $("#synthesisResults").show()
          prettyPrint();
          $("#synthesisDialog .exploreButton").show()
          $("#synthesisDialog .importButton").show()
          $("#synthesisDialog .importButton").unbind("click").click(function () {
              handlers("replace_code")({ newCode: data.allCode })
              if ("cursor" in data) {
                setTimeout(function() {
                  handlers("move_cursor")(data.cursor)
                }, 100);
              }
          })
          $("#synthesisDialog .exploreButton").unbind("click").click(function () {
            var cid    = 1*$("#synthesisDialog").attr("cid")
            var fname  = $("#synthesisDialog").attr("fname")

            $("#synthesisDialog").modal("hide")

            var msg = JSON.stringify(
              {action: "doExplore", module: "synthesis", fname: fname, cid: cid, "explore-action": "init", path: [], ws: 0}
            )

            leonSocket.send(msg)
          })
          $("#synthesisDialog .cancelButton").hide()
          $("#synthesisDialog .closeButton").show()
          synthesizing = false;
      }
  }

  handlers("synthesis_exploration") = function(data) {
      var d = $("#synthesisExploreDialog");

      prettyPrint();

      if (!d.is(":visible")) {
          d.modal("show")
      }

      var working = false

      d.unbind("hide.bs.modal").on("hide.bs.modal", function () {
        if (working) {
          var msg = JSON.stringify({
              module: "main",
              action: "doCancel"
          })

          leonSocket.send(msg)
        }
      })

      var node = d.find(".exploreBlock[path=\""+data.from.join("-")+"\"]")
      node.replaceWith(data.html)
      prettyPrint();

      var wsOf = function(e) {
        var b = $(e).closest(".exploreBlock")
        return 1*b.attr("ws");
      }

      var pathOf = function(e) {
        var b = $(e).closest(".exploreBlock")
        var path = []
        if (b.attr("path") != "") {
          path = b.attr("path").split("-").map(function(e) {
            return 1*e;
          })
        }
        return path;
      }

      d.find("""select[data-action="select-alternative"]""").unbind("change").change(function() {

        $(this).after(""" <span class="fa fa-spin fa-circle-o-notch"></span>""");
        var msg = JSON.stringify(
          {action: "doExplore", module: "synthesis", fname: data.fname, cid: data.cid, path: pathOf(this), ws: wsOf(this),
           "explore-action": $(this).attr("data-action"),
           select: 1*$(this).value()
          }
        )

        leonSocket.send(msg)
      });

      d.find("span.knob").unbind("click").click(function() {
        $(this).removeClass("fa-arrow-right", "fa-arrow-left").addClass("fa-spin fa-refresh")

        var msg = JSON.stringify(
          {action: "doExplore", module: "synthesis", fname: data.fname, cid: data.cid, path: pathOf(this), ws: wsOf(this),
           "explore-action": $(this).attr("data-action")
          }
        )


        leonSocket.send(msg)

        working = true
      });

      d.find(".importButton").unbind("click").click(function () {
          handlers("replace_code")({ newCode: data.allCode })
          if ("cursor" in data) {
            setTimeout(function() {
              handlers("move_cursor")(data.cursor)
            }, 100);
          }
      })
  }

  handlers("synthesis_rulesToApply") = function(data) {
      var fname       = data.fname
      var cid         = data.cid
      var rulesApps   = data.rulesApps

      var html = "";

      // Start by removing temp content
      if (compilationStatus == 1) {
          for (var i = 0; i < rulesApps.length; i++) {
              var app = rulesApps[i];
              var statusIcon = ""
              var clazz = "temp"

              if (app.status == "closed") {
                  statusIcon = """<i class="fa fa-exclamation-circle"></i> """
                  clazz += " disabled"
              }
              html += """<li role="presentation" class=""""+clazz+""""><a role="menuitem" tabindex="-1" href="#" action="rule" cid=""""+cid+"""" rid=""""+app.id+"""">"""+statusIcon+app.name+"""</a></li>"""
          }
      } else {
          html += """<li role="presentation" class="temp disabled"><a role="menuitem" tabindex="-1" href="#" fname=""""+fname+"""">Not yet compiled...</a></li>"""
      }

      var selector = "#synthesis .problem[fname=\""+fname+"\"][cid=\""+cid+"\"] ul"
      $(selector+" li.temp").remove()
      $(selector).append(html)
      $(selector+" li a[action=\"search\"]").unbind("click").click(function() {
          var msg = JSON.stringify(
            {action: "doSearch", module: "synthesis", fname: fname, cid: cid}
          )

          leonSocket.send(msg)
      })
      $(selector+" li a[action=\"explore\"]").unbind("click").click(function() {
          var msg = JSON.stringify(
            {action: "doExplore", module: "synthesis", fname: fname, cid: cid, "explore-action": "init", path: [], ws: 0}
          )

          leonSocket.send(msg)
      })
      $(selector+" li a[action=\"rule\"]").click(function() {
          var rid = 1*$(this).attr("rid")

          var msg = JSON.stringify(
            {action: "doApplyRule", module: "synthesis",  fname: fname, cid: cid, rid: rid}
          )

          leonSocket.send(msg)
      })
  }

  handlers("repair_result") = function(data) {
      var pb = $("#repairProgress")
      var pbb = $("#repairProgress .progress-bar")

      // setup and open pane
      if (data.result == "init") {
          $("#repairResults").hide()
          $("#repairFocused").hide()
          $("#repairDialog .importButton").hide()
          $("#repairDialog .closeButton").hide()
          $("#repairDialog .cancelButton").show()
          $("#repairDialog").modal("show")

          $("#repairDialog").unbind("hide.bs.modal").on("hide.bs.modal", function () {
              if (synthesizing) {
                  var msg = JSON.stringify({
                      module: "repair",
                      action: "doCancel"
                  })

                  leonSocket.send(msg)
              }
          })
      } else if (data.result == "progress") {
          pbb.addClass("active progress-bar-striped")
          pbb.removeClass("progress-bar-success progress-bar-danger")
          pbb.width("100%")
          pbb.html(data.progress);

          $("#repairProgressBox").show()
      } else if (data.result == "error") {
          pbb.removeClass("active progress-bar-striped")

          pbb.width("100%")
          pbb.html(data.error);
          pbb.addClass("progress-bar-danger")

          $("#repairDialog .cancelButton").hide()
          $("#repairDialog .closeButton").show()
      } else if (data.result == "focused") {
          $("#repairFocused .code.focused").removeClass("prettyprinted")
          $("#repairFocused .code.focused").html(data.focused)
          $("#repairFocused").show()
          prettyPrint();
      } else if (data.result == "success") {
          pbb.removeClass("active progress-bar-striped")

          pbb.width("100%")
          pbb.html(data.success);
          pbb.addClass("progress-bar-success")

          $("#repairResults .code.solution").removeClass("prettyprinted")
          $("#repairResults .code.solution").html(data.solCode)
          $("#repairResults").show()
          prettyPrint();
          $("#repairDialog .importButton").show()
          $("#repairDialog .importButton").unbind("click").click(function () {
              handlers("replace_code")({ newCode: data.allCode })
              if ("cursor" in data) {
                setTimeout(function() {
                  handlers("move_cursor")(data.cursor)
                }, 100);
              }
          })
          $("#repairDialog .cancelButton").hide()
          $("#repairDialog .closeButton").show()
      }
  }

  function displayVerificationDetails(status, vcs) {
      var pb = $("#verifyProgress")
      var pbb = pb.children(".progress-bar")

      pbb.width("100%")
      pb.removeClass("active")
      pb.addClass("progress-bar-striped")

      pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

      var canRepair = false

      switch (status) {
          case "cond-valid":
              pbb.html("Conditionally Valid!")
              pbb.addClass("progress-bar-warning")
              break;

          case "valid":
              pbb.html("Valid!")
              pbb.addClass("progress-bar-success")
              break;

          case "invalid":
              pbb.html("Invalid!")
              pbb.addClass("progress-bar-danger")
              canRepair = true
              break;

          case "unknown":
              pbb.html("Unknown ?!")
              pbb.addClass("progress-bar-warning")
              break;

          case "timeout":
              pbb.html("Timeout!")
              pbb.addClass("progress-bar-warning")
              break;
      }

      var tbl = $("#verifyResults tbody")
      tbl.html("");

      for (var i = 0; i < vcs.length; i++) {
          var vc = vcs[i];
          var icon = "check"
          if (vc.status == "invalid") {
              icon = "warning"
          } else if (vc.status == "unknown") {
              icon = "question"
          } else if (vc.status == "timeout") {
              icon = "clock-o"
          }

          var clas = "success"
          if (vc.status == "invalid") {
            clas = "danger"
          } else if (vc.status == "unknown" || vc.status == "timeout") {
            clas = "warning"
          }


          tbl.append("<tr class=\""+clas+"\"> <td>"+vc.fun+"</td> <td>"+vc.kind+"</td> <td><i class=\"fa fa-"+icon+"\"></i> "+vc.status+"</td> <td>"+vc.time+"</td> </tr>")

          if ("counterExample" in vc) {
              var html = "<tr class=\""+clas+" counter-example\"><td colspan=\"4\">"
              html += "<div>"
              html += "  <p>The following inputs violate the VC:</p>";
              html += "  <table class=\"input\">";
              for (var v in vc.counterExample) {
                  html += "<tr><td>"+v+"</td><td>&nbsp;:=&nbsp;</td><td>"+vc.counterExample[v]+"</td></tr>";
              }
              html += "  </table>"

              if ("execution" in vc && vc.execution.result == "success" && features["execution"].active) {
                  html += "  <p>It produced the following output:</p>";
                  html += "  <div class=\"output\">"+vc.execution.output+"</div>"
              }

              html += "    </div>"
              html += "  </td>"
              html += "</tr>"

              tbl.append(html)
          }
      }

      if (vcs.length == 0) {
          tbl.append("<tr class=\"empty\"><td colspan=\"4\"><div>No VC found</div></td></tr>")
      }

      $("div[aria-describedby='verifyDialog'] span.ui-button-text").html("Close")

      if (canRepair && features["repair"].active) {
        $(".repairButton").unbind("click").click(function () {
            var fname = vc.fun

            var msg = JSON.stringify(
              {action: "doRepair", module: "repair", fname: fname}
            )

            leonSocket.send(msg)

            $("#verifyDialog").modal("hide")
        });
        $(".repairButton").show();
      } else {
        $(".repairButton").hide();
      }

      $("#verifyResults").show("fade");

  }

  handlers("verification_result") = function (data) {
      displayVerificationDetails(data.status, data.vcs)
  }

  function displayTerminationDetails(status, fdata) {
      var pb = $("#terminationProgress")
      var pbb = pb.children(".progress-bar")

      pbb.width("100%")
      pb.removeClass("active")
      pb.addClass("progress-bar-striped")

      pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

      var tbl = $("#terminationResults table")
      tbl.html("");

      switch (status) {
          case "terminates":
              pbb.html("Terminates!")
              pbb.addClass("progress-bar-success")
              tbl.append("""<tr class="success"> <td>This function terminates for all inputs.</td> </tr>""");
              break;

          case "loopsfor":
              pbb.html("Non-terminating!")
              pbb.addClass("progress-bar-danger")
              var html = """<tr class="danger counter-example"><td><div>"""
              html += "<p>The function does not terminate for the following call:</p>";
              html += "<table class=\"input\">";
              html += "  <tr><td>"+fdata.call+"</td></tr>";
              html += "</table>"
              html += "</div></td></tr>"
              tbl.append(html);
              break;

          case "callsnonterminating":
              pbb.html("Calls non-terminating functions!")
              pbb.addClass("progress-bar-warning")
              var html = """<tr class="warning counter-example"><td><div>"""
              html += "<p>The function calls the following non-terminating function(s):</p>";
              html += "<table class=\"input\">";
              for (var i = 0; i < fdata.calls.length; i++) {
                  html += "<tr><td>"+fdata.calls[i]+"</td></tr>";
              }
              html += "</table>"
              html += "</div></td></tr>"
              tbl.append(html);
              break;

          case "noguarantee":
              pbb.html("No guarantee!")
              pbb.addClass("progress-bar-warning")
              tbl.append("""<tr class="warning"> <td>Leon could not determine whether or not this function terminates.</td> </tr>""");
              break;

          default:
              pbb.html("Unknown!")
              pbb.addClass("progress-bar-warning")
              break;
      }

      $("div[aria-describedby='terminationDialog'] span.ui-button-text").html("Close")
      $("#terminationResults").show("fade");
  }

  function error(msg) {
      alert(msg);
  }

  var receiveEvent = function(event) {
      var data = JSON.parse(event.data)
      if (data.kind in handlers) {
          handlers[data.kind](data);
      } else {
          console.log("Unknown event type: "+data.kind)
          console.log(data)
      }
  }

  var connected = false

  var lastReconnectDelay = 0;
  var reconnectIn = 0;

  var closeEvent = function(event) {
      if (connected) {
          setDisconnected()
      }
  }

  var openEvent = function(event) {
      if (lastReconnectDelay != 0) {
        notify("And we are back online!", "success")
        updateCompilationStatus("unknown")
        oldCode = ""
      }

      setConnected()

      for (var f in features) {
          var msg = JSON.stringify(
            {action: "featureSet", module: "main", feature: f, active: features[f].active}
          )

          leonSocket.send(msg)
      }

      if(hash) {
          loadStaticLink(hash)
      } else {
          recompile()
      }
  }

  function loadStaticLink(hash) {
      if (hash.indexOf("#link/") == 0) {
          var msg = JSON.stringify(
            {action: "accessPermaLink", module: "main", link: hash.substr("#link/".length)}
          )

          leonSocket.send(msg);
          window.location.hash = "";
      }
      if (hash.indexOf("#demo/") == 0) {
          loadExample("demo", hash.substr("#demo/".length))
          window.location.hash = "";
      }
  }

  $(window).on("hashchange", function() {
      var hash = window.location.hash;
      loadStaticLink(hash);
  });

  function setDisconnected() {
      connected = false
      updateCompilationStatus("disconnected")
      lastReconnectDelay = 5;
      reconnectIn = lastReconnectDelay;

      checkDisconnectStatus()
  }

  function setConnected() {
      connected = true

      $("#connectError").hide();
      $("#disconnectError").hide();

      lastReconnectDelay = 0;
      reconnectIn = -1;
  }

  function checkDisconnectStatus() {
      if (reconnectIn == 0) {
          reconnectIn = -1;
          $("#disconnectError #disconnectMsg").html("Attempting reconnection...");

          connectWS()

          // If still not connected after 2 seconds, consider failed
          setTimeout(function() {
              if (!connected) {
                  if (lastReconnectDelay == 0) {
                      lastReconnectDelay = 5;
                  } else {
                      lastReconnectDelay *= 2;
                  }

                  reconnectIn = lastReconnectDelay;
              }
          }, 2000);
      } else if (reconnectIn > 0) {
          $("#disconnectError #disconnectMsg").html("Retrying in "+reconnectIn+""" seconds... <button id="tryReconnect" class="btn btn-danger btn-xs">Try now</button>""");

          $("#tryReconnect").click(function() {
              reconnectIn = 0;
              checkDisconnectStatus();
          })

          $("#disconnectError").show().alert();

          reconnectIn -= 1;
      }
  }

  setInterval(function () { checkDisconnectStatus() }, 2000);

  var errorEvent = function(event) {
      console.log("ERROR")
      console.log(event)
  }

  connectWS()
  setTimeout(function() {
      if (!connected) {
          $("#disconnectError").hide();
          $("#connectError").show().alert();
      }
  }, 3000);

  function connectWS() {
      leonSocket = js.Dynamic.newInstance(_leon_websocket_url)
      leonSocket.onopen = openEvent
      leonSocket.onmessage = receiveEvent;
      leonSocket.onclose = closeEvent
      leonSocket.onerror = errorEvent
  }

  var lastChange      = 0;
  var lastSavedChange = lastChange;
  var timeWindow      = 2000;

  function updateSaveButton() {
      var e = $("#button-save")
      if (lastChange == lastSavedChange) {
         e.addClass("disabled"); 
      } else {
         e.removeClass("disabled"); 
      }
  }

  def notify(content, type, fade) {
      if (!fade) {
          fade = 3000
      }

      if (type == "error") {
        type = "danger"
      }

      var note = $("<div>", {
          "class": "alert fade in alert-"+type
      }).html("""<button type="button" class="close" data-dismiss="alert">×</button>"""+content)

      $("#notifications").append(note);

      setTimeout(function() {
          note.hide();
      }, fade)
  }

  var oldCode = ""

  def recompile() = {
      var currentCode = editor.getValue()

      if (oldCode != "" && oldCode != currentCode) {
          if (forwardChanges.length == 0) {
              storeCurrent(oldCode)
          }
      }

      if (connected && oldCode != currentCode) {
          var msg = JSON.stringify(
            {action: "doUpdateCode", module: "main", code: currentCode}
          )
          oldCode = currentCode;
          lastSavedChange = lastChange;
          updateSaveButton();
          leonSocket.send(msg)
          updateCompilationStatus("unknown")
          updateCompilationProgress(0)
      }
  }

  function onCodeUpdate() {
      var now = new Date().getTime()

      if (lastChange < (now - timeWindow)) {
          if(lastChange > 0) { 
            recompile()
          }
          lastChange = new Date().getTime();
      }

      localStorage.setItem("leonEditorCode", editor.getValue());
  }

  function loadSelectedExample() {
      var selected = $("""#example-loader""").find(":selected[id]")

      var id = selected.attr("id")
      var group = selected.attr("group")

      loadExample(group, id)
  }
  function loadExample(group, id) {
      if (id) {
          $.ajax({
            url: "/ajax/getExample/"+group+"/"+id,
            dataType: "json",
            success: function(data, textStatus, jqXHR) {
              if (data.status == "success") {
                  storeCurrent(editorSession.getValue())
                  editor.setValue(data.code);
                  editor.selection.clearSelection();
                  editor.gotoLine(0);
                  recompile();
                  $("#example-loader").get(0).selectedIndex = 0;
              } else {
                  notify("Loading example failed :(", "error")
              }
            },
            error: function(jqXHR, textStatus, errorThrown) {
              notify("Loading example failed :(", "error")
            }
          });
      }
  }

  $("#example-loader").change(loadSelectedExample);

  var editorSession = editor.getSession();

  editor.commands.addCommand({
      name: "save",
      bindKey: {win: "Ctrl-S",  mac: "Command-S"},
      exec: function(editor) {
          recompile()
      },
      readOnly: true
  });

  editor.commands.removeCommand("replace");
  editor.commands.removeCommand("transposeletters");

  editorSession.on("change", function(e) {
      lastChange = new Date().getTime();
      updateSaveButton();
      setTimeout(onCodeUpdate, timeWindow+50)
  });

  function resizeEditor() {

      var h = $(window).height()-$("#title").height()-6
      var ah = $("#annotations").height()
      var w = $("#codecolumn").width()

      $("#codecolumn").height(h);
      $("#panelscolumn").height(h);
      $("#leoninput").height(h-ah).width(w);
      $("#codebox").height(h-ah).width(w);

      editor.resize();
  };

  $(window).resize(resizeEditor);

  resizeEditor();


  handlers("replace_code") = function(data) {
      storeCurrent(editorSession.getValue())
      editorSession.setValue(data.newCode)
      recompile()
  }

  var currentMousePos = { x: -1, y: -1 };

  $(document).mousemove(function(event) {
      currentMousePos = { x: event.pageX, y: event.pageY };
  });

  function openVerifyDialog() {
      $("#verifyDialog").modal("show")
  }

  function openTerminationDialog() {
      $("#terminationDialog").modal("show")
  }

  var storedCode = localStorage.getItem("leonEditorCode")

  var seenDemo = 1*localStorage.getItem("leonSeenDemo")
  var demos = [
      {
          placement: "modal",
          title: "Welcome to Leon!",
          content: "Leon is an automated system for <strong>synthesizing</strong> and <strong>verifying</strong> functional Scala programs."
      },
      {
          where: function() { return $("#example-loader") },
          placement: "left",
          title: "Select from examples",
          content: "You can try <em>Leon</em> on a list of selected examples, covering both synthesis and verification problems."
      },
      {
          where: function() { return $($(".ace_line_group")[13]).find("span").last() },
          placement: "right",
          title: "Edit at will",
          content: "Feel free to modify or extend the selected examples with your own code."
      },
      {
          where: function() { return $("#overview_table") },
          placement: "left",
          title: "Live results",
          content: "Leon will verify your code in the background and display live verification results here."
      },
      {
          where: function() { return $($("#overview_table td.status.verif")[2]) },
          placement: "left",
          title: "Display details",
          content: "Click on the verification status of each function to get more information!"
      },
      {
          where: function() { return $("#synthesis_table td.problem").first() },
          placement: "left",
          title: "Synthesize",
          content: "Click on a synthesis problem to solve it! You can either ask <em>Leon</em> to <strong>search</strong> for a solution, or perform individual steps yourself."
      },
      {
          where: function() { return $("#button-permalink") },
          placement: "bottom",
          title: "Permalinks",
          content: "You can generate permalinks to the editor session. If you experience any problem with the interface or if you do not understand the result, send us a link!"
      }
  ];

  if (!seenDemo || (seenDemo < demos.length-1)) {

      var lastDemo = null

      function showDemo(id) {
          var demo = demos[id]

          if (demo.placement == "modal") {
              // Assume only the first demo is modal
              var html  = """<div id="demoPane" class="modal fade" tabindex="-1" role="dialog" aria-labelledby="demoModal" aria-hidden="true" data-backdrop="static">"""
              html     += """  <div class="modal-dialog">"""
              html     += """    <div class="modal-content">"""
              html     += """      <div class="modal-header">"""
              html     += """        <button type="button" class="close" demo-action="close" data-dismiss="modal" aria-hidden="true">×</button>"""
              html     += """        <h3 id="demoModal">"""+demo.title+"""</h3>"""
              html     += """      </div>"""
              html     += """      <div class="modal-body">"""
              html     += """        """+demo.content
              html     += """      </div>"""
              html     += """      <div class="modal-footer">"""
              html     += """        <button class="btn btn-success" data-dismiss="modal" aria-hidden="true" demo-action="next">Take the tour <i class="fa fa-play"></i></button>"""
              html     += """        <button class="btn" data-dismiss="modal" aria-hidden="true" demo-action="close">No thanks</button>"""
              html     += """      </div>"""
              html     += """    </div>"""
              html     += """  </div>"""
              html     += """</div>"""

              $("body").append(html);

              $("#demoPane").modal("show")

              var action = "close"

              $("#demoPane button").click(function() {
                  action = $(this).attr("demo-action")
                  hideDemo(id)
              })

              $("#demoPane").unbind("hide.bs.modal").on("hide.bs.modal", function () {
                  if (action == "next") {
                      localStorage.setItem("leonSeenDemo", id+1)
                      setTimeout(function() { showDemo(id+1) }, 500)
                  } else {
                      localStorage.setItem("leonSeenDemo", 100)
                  }
              })

          } else {
              var content = """<div id="demoPane" class="demo">"""
              content += demo.content
              content += """  <div class="demo-nav">"""
              if (id == demos.length-1) {
                  // last demo
                  content += """    <button class="btn btn-success" demo-action="close">Ok!</button>""";
              } else {
                  content += """    <button class="btn" demo-action="close">Got it</button>""";
                  content += """    <button class="btn btn-success" demo-action="next">Next <i class="fa fa-forward"></i></button>""";
              }
              content += """  </div>"""
              content += """</div>"""

              var where = demo.where()

              lastDemo = where;

              if (where.length == 0) {
                localStorage.setItem("leonSeenDemo", id+1)
                hideDemo(id)
                showDemo(id+1)
                return;
              }

              var progress = ""
              for (var i = 0; i < demos.length-1; i++) {
                  if (i < id) {
                      progress += """<i class="fa fa-circle"></i>"""
                  } else {
                      progress += """<i class="fa fa-circle-o"></i>"""
                  }
              }

              where.popover({
                  html: true,
                  placement: demo.placement,
                  trigger: "manual",
                  title: """<span class="demo-progress">"""+progress+"""</span>"""+demo.title,
                  content: content,
                  container: "body"
              })

              where.popover("show")

              $("#demoPane button[demo-action=\"close\"]").click(function() {
                  localStorage.setItem("leonSeenDemo", 100)
                  hideDemo(id)
              })

              $("#demoPane button[demo-action=\"next\"]").click(function() {
                  localStorage.setItem("leonSeenDemo", id+1)
                  hideDemo(id)
                  showDemo(id+1)
              })
          }


      }

      function hideDemo(id) {
          var demo = demos[id]

          if (demo.placement == "modal") {
              $("#demoPane").modal("hide")
              $("#demoPane").unbind("hidden").on("hidden", function() { $("demoPane").remove() })
          } else {
              lastDemo.popover("destroy")
          }
      }

      var toShow = seenDemo? seenDemo : 0;
      if (toShow != 0) {
          setTimeout(function() { showDemo(toShow) }, 1000)
      } else {
          showDemo(toShow)
      }

      storedCode = null
  }

  if (storedCode != null) {
    editor.setValue(storedCode);
    editor.selection.clearSelection();
    editor.gotoLine(0);
  }

  /*
  snowStorm.snowColor = "#ddddff";
  snowStorm.vMaxX = 2;
  snowStorm.vMaxY = 2;
  snowStorm.useTwinkleEffect = false;
  snowStorm.flakesMinActive = 350;
  snowStorm.flakesMaxActive = 350;
  snowStorm.followMouse = false;
  snowStorm.stop();
  */
  */
}