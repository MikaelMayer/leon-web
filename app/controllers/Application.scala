package leon.web
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import models.FileExamples
import models.LeonConsole

object Application extends Controller {

  val allExamples = List(
    "Synthesis"    -> new FileExamples("synthesis").allExamples,
    "Verification" -> new FileExamples("verification").allExamples,
    "Tutorials"    -> new FileExamples("tutorials").allExamples
  )

  val tutorialExamples = allExamples.filter(_._1 == "Tutorials")
  val otherExamples    = allExamples.filter(_._1 != "Tutorials")

  def index = Action { implicit request =>
    val prefix = Play.current.configuration.getString("app.prefix").getOrElse("")
    val url    = Play.current.configuration.getString("app.url").getOrElse("/")

    Ok(views.html.index(otherExamples, otherExamples.head._2(1), prefix, url))
  }

  def tutorials = Action { implicit request =>
    val prefix = Play.current.configuration.getString("app.prefix").getOrElse("")
    val url    = Play.current.configuration.getString("app.url").getOrElse("/")

    Ok(views.html.index(tutorialExamples, tutorialExamples.head._2.head, prefix, url))
  }

  def getExample(kind: String, id: Int) = Action { 
    allExamples.toMap.get(kind).flatMap(_.lift.apply(id)) match {
      case Some(ex) =>
        Ok(toJson(Map("status" -> "success", "code" -> ex.code)))
      case None =>
        Ok(toJson(Map("status" -> "error", "errormsg" -> "Unknown example")))
    }
  }

  def openConsole() = WebSocket.async[JsValue] { request =>
    LeonConsole.open(request.remoteAddress)
  }

}
