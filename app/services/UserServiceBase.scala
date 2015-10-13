
package leon.web
package services

import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.User

import scala.concurrent.Future

abstract class UserServiceBase extends UserService[User] {

  override def saveToken(token: MailToken): Future[MailToken] = {
    throw new UnsupportedOperationException("UserService.saveToken is not supported")
  }

  override def findToken(token: String): Future[Option[MailToken]] = {
    throw new UnsupportedOperationException("UserService.findToken is not supported")
  }

  override def deleteToken(uuid: String): Future[Option[MailToken]] = {
    throw new UnsupportedOperationException("UserService.deleteToken is not supported")
  }

  override def deleteExpiredTokens(): Unit = {
    throw new UnsupportedOperationException("UserService.deleteExpiredTokens is not supported")
  }

  override def updatePasswordInfo(user: User, info: PasswordInfo): Future[Option[BasicProfile]] = {
    throw new UnsupportedOperationException("UserService.updatePasswordInfo is not supported")
  }

  override def passwordInfoFor(user: User): Future[Option[PasswordInfo]] = {
    throw new UnsupportedOperationException("UserService.passwordInfoFor is not supported")
  }

}

