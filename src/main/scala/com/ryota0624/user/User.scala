package com.ryota0624.user

import java.time.LocalDateTime

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import com.ryota0624.user.LoggedInUser.Command
import com.ryota0624.{ApplicationTime, CanBeEncrypted, Encrypted, user}

import scala.concurrent.duration._
import scala.util.{Failure, Success}


// Userはchat appの利用者です。
trait User {
  // TODO このトレイト本当に必要？って感じなので消す

  import com.ryota0624.user.User._

  def id: ID

  def name: Name

  def status: Status
}

object User {

  class ID(private val value: String) {
    override def toString: String = value
  }

  object ID {
    def apply(value: String): User.ID = new ID(value)

    def generate(): ID = ID(java.util.UUID.randomUUID().toString)
  }

  sealed trait Status

  case object Active extends Status

  case class Deleted(at: LocalDateTime) extends Status

  class Name(private val name: String)

  object Name {
    def apply(name: String): Name = new Name(name)
  }

}


// LoggedInUser はログインした利用者です。
case class LoggedInUser(
                         id: User.ID,
                         name: User.Name,
                         email: LoggedInUser.Email,
                         password: LoggedInUser.Password,
                         status: User.Status,
                       ) extends User {

  import com.ryota0624.user.LoggedInUser._

  def updateEmail(newEmail: Email, newPassword: Password): Either[InvalidLoginInfo, (LoggedInUser, EmailUpdated)] = {
    if (password != newPassword) Left(InvalidLoginInfo("password does not match"))
    else {
      val evt = EmailUpdated(id, newEmail)
      Right(apply(evt), evt)
    }
  }

  def delete(inputEmail: Email, inputPassword: Password, deletedAt: LocalDateTime): Either[InvalidLoginInfo, (LoggedInUser, Deleted)] = {
    if (inputEmail == email && inputPassword == password) {
      val evt = Deleted(id, deletedAt)
      Right((apply(evt), evt))
    } else Left(InvalidLoginInfo("password or email does not match"))
  }

  def apply(evt: Event): LoggedInUser = {
    evt match {
      case evt: EmailUpdated => applyEmailUpdated(evt)
      case evt: Deleted => applyDeleted(evt)
    }
  }

  private def applyEmailUpdated(evt: EmailUpdated) =
    copy(email = evt.email)

  private def applyDeleted(evt: Deleted) =
    copy(status = User.Deleted(evt.at))
}

object LoggedInUser {

  def name(id: User.ID): String = s"${id.toString}"

  sealed trait ValidationError
  // TODO: ValidationError てきなのはutilsにいい感じしときたい

  case class InvalidLoginInfo(message: String) extends ValidationError

  case object NotFoundUser extends ValidationError

  sealed trait Command {
    def id: User.ID

    def sender: ActorRef[Response]
  }

  final case class UpdateEmail(
                                id: User.ID,
                                email: LoggedInUser.Email,
                                password: Password,
                                sender: ActorRef[Response],
                              ) extends Command

  final case class Delete(
                           id: User.ID,
                           email: LoggedInUser.Email,
                           password: LoggedInUser.Password,
                           sender: ActorRef[Response],
                         ) extends Command

  case class Activate(id: User.ID, name: User.Name, email: Email, password: Password, sender: ActorRef[Response]) extends Command


  case class Response(id: User.ID, validationError: Option[ValidationError])

  object Response {
    def success(id: User.ID): Response = new Response(id, None)

    def failure(id: User.ID, validationError: ValidationError): Response = new Response(id, Some(validationError))
  }

  sealed trait Event {
    def id: User.ID
  }

  final case class EmailUpdated(
                                 id: User.ID,
                                 email: LoggedInUser.Email,
                               ) extends Event

  final case class Deleted(
                            id: User.ID,
                            at: LocalDateTime,
                          ) extends Event

  def apply()
           (implicit applicationTime: ApplicationTime): Behavior[Command] =
    waitActivate()

  private def waitActivate()(implicit applicationTime: ApplicationTime): Behavior[Command] =
    Behaviors.receive { (ctx, message) =>
      message match {
        case Activate(id, name, email, password, sender) =>
          sender ! Response.success(id)
          run(new LoggedInUser(id, name, email, password, User.Active))
        case _ =>
          ctx.log.warn("invalid msg received")
          Behaviors.ignore
      }
    }

  private def run(user: LoggedInUser)(implicit applicationTime: ApplicationTime): Behavior[Command] =
    Behaviors.receive { (ctx, message) =>
      if (message.id == user.id) message match {
        case UpdateEmail(_, email, password, sender) =>
          user.updateEmail(email, password) match {
            case Left(validationError: ValidationError) =>
              sender ! Response.failure(user.id, validationError)
              Behaviors.same
            case Right((updated, evt)) =>
              ctx.system.eventStream.tell(EventStream.Publish(evt))
              sender ! Response.success(user.id)
              run(updated)
          }
        case Delete(_, email, password, sender) =>
          user.delete(email, password, applicationTime.now()) match {
            case Left(validationError: ValidationError) =>
              sender ! Response.failure(user.id, validationError)
              Behaviors.same
            case Right((updated, evt)) =>
              ctx.system.eventStream.tell(EventStream.Publish(evt))
              sender ! Response.success(updated.id)
              run(user)
          }
        case _: Activate =>
          ctx.log.warn("invalid msg received")
          Behaviors.ignore
      } else Behaviors.same
    }

  class Email(private val value: String)

  class Password(private val plainText: String) extends CanBeEncrypted {
    protected override def toPlainText: String = plainText
  }

  class EncryptedPassword(private val value: String) extends Encrypted[Password] {
    override def decrypt(): Password = ???
  }

}

// AnonymousUser はログインしていない利用者です。
case class AnonymousUser(
                          id: User.ID,
                          name: User.Name,
                          status: User.Status,
                        ) extends User

object AnonymousUser {

  def apply(id: User.ID): AnonymousUser = new AnonymousUser(id, generateRandomName(), User.Active)

  def generateRandomName(): User.Name = {
    User.Name(pickRandomNameColor ++ "色の" ++ pickRandomNameAnimal)
  }

  private def pickRandomNameColor = nameVariationColor(scala.util.Random.nextInt(nameVariationColor.size))

  private def pickRandomNameAnimal = nameVariationAnimal(scala.util.Random.nextInt(nameVariationAnimal.size))


  private val nameVariationColor = Seq(
    "赤",
    "白",
    "黄色",
    "青"
  )

  private val nameVariationAnimal = Seq(
    "犬",
    "ねこ",
    "ねずみ",
    "トナカイ"
  )
}

case class Users(anonymousUsers: Seq[AnonymousUser], loggedInUsers: Map[User.ID, ActorRef[user.LoggedInUser.Command]]) {
  def registerUser(id: User.ID, userRef: ActorRef[Command]): Users = copy(loggedInUsers = loggedInUsers + ((id, userRef)))

  def addAnonymousUser(anonymousUser: AnonymousUser): Users = copy(anonymousUsers = anonymousUsers :+ anonymousUser)
}

object Users {

  def apply()(implicit applicationTime: ApplicationTime): Behavior[Command] =
    run(new Users(Seq.empty, Map.empty))


  sealed trait Command

  case class RegisterUser(name: User.Name, email: LoggedInUser.Email, password: LoggedInUser.Password, sender: Option[ActorRef[Response]] = None) extends Command

  case class UserCommand(cmd: LoggedInUser.Command) extends Command

  case class UserResponse(response: LoggedInUser.Response, replayTo: Option[ActorRef[Response]]) extends Command

  case object RegisterAnonymousUser extends Command

  case class Response(validationError: Option[String]) // TODO ValidationErrorにすべし

  object Response {
    def success(): Response = Response(None)
    def failure(validationError: String): Response = Response(Some(validationError))
  }

  sealed trait Event

  case class UserRegistered(id: User.ID) extends Event

  case class AnonymousUserRegistered(id: User.ID) extends Event

  implicit val askTimeout: Timeout = Timeout(2.second)

  private def run(users: Users)(implicit t: ApplicationTime): Behavior[Command] = Behaviors.receive {
    (ctx, message) => {
      // val loggedInUserResponseAdapter = ctx.messageAdapter(WrappedLoggedInUserResponse)

      message match {
        case RegisterUser(name, email, password, sender) =>
          val id = User.ID.generate()
          val user = LoggedInUser()
          val userRef = ctx.spawn(user, LoggedInUser.name(id))
          ctx.ask[LoggedInUser.Command, LoggedInUser.Response](userRef, LoggedInUser.Activate(id, name, email, password, _)) {
            case Failure(exception) =>
              // TODO senderに返してやる
              ctx.log.error(s"failure! $exception", exception)
              ???
            case Success(value) =>
              ctx.log.info(s"response! $value")
              UserResponse(value, sender)
          }
          ctx.system.eventStream ! EventStream.Publish(UserRegistered(id))
          run(users.registerUser(id, userRef))
        case RegisterAnonymousUser =>
          val id = User.ID.generate()
          val user = AnonymousUser(id)
          val evt = AnonymousUserRegistered(id)
          ctx.system.eventStream.tell(EventStream.Publish(evt))
          run(users.addAnonymousUser(user))
        case UserResponse(response, sender) =>
          response.validationError match {
            case Some(value) => sender.foreach(_ ! Response.failure(value.toString))
            case None => sender.foreach(_ ! Response.success())
          }
          Behaviors.ignore
        case UserCommand(command) =>
          users.loggedInUsers.get(command.id) match {
            case Some(user) => user ! command
            case None => command.sender ! LoggedInUser.Response.failure(command.id, LoggedInUser.NotFoundUser)
          }
          Behaviors.same
      }
    }
  }
}