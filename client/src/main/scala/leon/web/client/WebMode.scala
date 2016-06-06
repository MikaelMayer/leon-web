package leon.web
package client

import org.scalajs.jquery.{ jQuery => $ }

import JQueryExtended.toJQueryExtended

object WebMode {
  
  var codeColumnOldClass = ""
  val codecolumnNewClass = "col-lg-3 col-sm-3 col-xs-12"
  def activate() = {
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").addClass("webdesign", 800)
    codeColumnOldClass = $("#codecolumn").attr("class").getOrElse("")
    $("#codecolumn").switchClass(codeColumnOldClass, codecolumnNewClass, 800)
    $("#htmlDisplayerDiv").addClass("webdesigninitial")
    $("#htmlDisplayerDiv").show()
    scalajs.js.timers.setTimeout(800) {
      $("#htmlDisplayerDiv").removeClass("webdesigninitial", 400)
    }
  }
  def deactivate() = {
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").removeClass("webdesign", 800)
    $("#codecolumn").switchClass(codecolumnNewClass, codeColumnOldClass, 800)
    $("#htmlDisplayerDiv").addClass("webdesigninitial", 400)
    scalajs.js.timers.setTimeout(800) {
      $("#htmlDisplayerDiv").hide()
    }
  }
}