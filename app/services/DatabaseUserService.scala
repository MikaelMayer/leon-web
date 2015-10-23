package leon.web
package services

import play.api.Logger
import play.api.db._
import play.api.Play.current
import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.{ User, UserId, ProviderId, Email }
import leon.web.repositories.UserRepository

import scala.concurrent.Future

class DatabaseUserService extends UserServiceBase {

  implicit val conn = DB.getConnection()

  val logger = Logger("services.UserService")

  override def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    logger.debug("find(%s, %s)".format(providerId, userId))
    Future.successful {
      UserRepository.findByProviderAndId(ProviderId(providerId), UserId(userId)).map(_.profile)
    }
  }

  override def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    logger.debug("findByEmailAndProvider(%s, %s)".format(email, providerId))
    Future.successful {
      UserRepository.findByEmailAndProvider(Email(email), ProviderId(providerId)).map(_.profile)
    }
  }

  private def findProfile(p: BasicProfile): Option[((String, String), User)] = {
    logger.debug("findProfile(%s)".format(p))
    UserRepository.findById(UserId(p.userId)).map { user =>
      (p.providerId, p.userId) -> user
    }
  }

  private def updateProfile(profile: BasicProfile, entry: ((String, String), User)): Future[User] = {
    logger.debug("updateProfile(%s, %s)".format(profile, entry))
    Future.successful {
      entry._2
    }
  }

  override def save(user: BasicProfile, mode: SaveMode): Future[User] = {
   logger.debug("save(%s, %s)".format(user, mode))
    Future.successful {
      UserRepository.save(User(user))
    }
  }

  override def link(current: User, to: BasicProfile): Future[User] = {
    Future.successful {
      current
    }
  }

}

