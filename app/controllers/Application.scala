package leon.web
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import examples._
import models.LeonConsole

object Application extends Controller {

  val examples = VerificationExamples.allExamples

  def index = Action { implicit request =>
    Ok(views.html.index(examples, VerificationExamples.default))
  }

  def getExample(id: Int) = Action { 
    examples.lift.apply(id) match {
      case Some(ex) =>
        Ok(toJson(Map("status" -> "success", "code" -> ex.code)))
      case None =>
        Ok(toJson(Map("status" -> "error", "errormsg" -> "Unknown example")))
    }
  }

  def openConsole() = WebSocket.async[JsValue] { request =>
    LeonConsole.open
  }

}
