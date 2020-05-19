package com.ryota0624.user

import java.time.LocalDateTime

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.Behaviors
import com.ryota0624.user.LoggedInUser.LoggedInUserCommand
import com.ryota0624.{ApplicationTime, CanBeEncrypted, Encrypted}


// Userはchat appの利用者です。
trait User {

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

  def updateEmail(newEmail: Email, newPassword: Password): Either[InvalidLoginInfo, LoggedInUser] = {
    if (password != newPassword) Left(InvalidLoginInfo("password does not match"))
    else Right(copy(email = newEmail))
  }

  def delete(inputEmail: Email, inputPassword: Password, deletedAt: LocalDateTime): Either[InvalidLoginInfo, LoggedInUser] = {
    if (inputEmail == email && inputPassword == password) Right(copy(status = User.Deleted(deletedAt)))
    else Left(InvalidLoginInfo("password or email does not match"))
  }
}

object LoggedInUser {

  def name(id: User.ID): String = s"${id.toString}"

  sealed trait ValidationError

  case class InvalidLoginInfo(message: String) extends ValidationError

  case object NotFoundUser extends ValidationError

  sealed trait LoggedInUserCommand {
    def id: User.ID

    def sender: ActorRef[UserCommandResponse]
  }

  final case class UpdateEmail(
                                id: User.ID,
                                email: LoggedInUser.Email,
                                password: Password,
                                sender: ActorRef[UserCommandResponse],
                              ) extends LoggedInUserCommand

  final case class Delete(
                           id: User.ID,
                           email: LoggedInUser.Email,
                           password: LoggedInUser.Password,
                           sender: ActorRef[UserCommandResponse],
                         ) extends LoggedInUserCommand

  sealed trait LoggedInUserResponse {
    def id: User.ID
  }

  case class UserCommandResponse(id: User.ID, validationError: Option[ValidationError])

  object UserCommandResponse {
    def success(id: User.ID): UserCommandResponse = new UserCommandResponse(id, None)

    def failure(id: User.ID, validationError: ValidationError): UserCommandResponse = new UserCommandResponse(id, Some(validationError))
  }

  sealed trait LoggedInUserEvent {
    def id: User.ID
  }

  final case class EmailUpdated(
                                 id: User.ID,
                                 email: LoggedInUser.Email,
                               ) extends LoggedInUserEvent

  final case class Deleted(
                            id: User.ID,
                          ) extends LoggedInUserEvent

  def apply(id: User.ID, name: User.Name, email: Email, password: Password)
           (implicit applicationTime: ApplicationTime): Behavior[LoggedInUserCommand] =
    run(new LoggedInUser(id, name, email, password, User.Active))

  private def run(user: LoggedInUser)(implicit applicationTime: ApplicationTime): Behavior[LoggedInUserCommand] =
    Behaviors.receive { (ctx, message) =>
      if (message.id == user.id) message match {
        case UpdateEmail(_, email, password, sender) =>
          user.updateEmail(email, password) match {
            case Left(validationError: ValidationError) =>
              sender ! UserCommandResponse.failure(user.id, validationError)
              Behaviors.same
            case Right(updated) =>
              val evt = EmailUpdated(updated.id, updated.email)
              sender ! UserCommandResponse.success(user.id)
              ctx.system.eventStream.tell(EventStream.Publish(evt))
              run(updated)
          }
        case Delete(_, email, password, sender) =>
          user.delete(email, password, applicationTime.now()) match {
            case Left(validationError: ValidationError) =>
              sender ! UserCommandResponse.failure(user.id, validationError)
              Behaviors.same
            case Right(updated) =>
              val evt = Deleted(updated.id)
              sender ! UserCommandResponse.success(user.id)
              ctx.system.eventStream.tell(EventStream.Publish(evt))
              run(user)
          }
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

case class Users(anonymousUsers: Seq[AnonymousUser], loggedInUsers: Map[User.ID, ActorRef[LoggedInUserCommand]]) {
  def registerUser(id: User.ID, userRef: ActorRef[LoggedInUserCommand]): Users = copy(loggedInUsers = loggedInUsers + ((id, userRef)))

  def addAnonymousUser(anonymousUser: AnonymousUser): Users = copy(anonymousUsers = anonymousUsers :+ anonymousUser)
}

object Users {

  def apply()(implicit applicationTime: ApplicationTime): Behavior[Command] =
    run(new Users(Seq.empty, Map.empty))

  import LoggedInUser._

  sealed trait Command

  case class RegisterUser(name: User.Name, email: Email, password: Password) extends Command

  case class WrappedLoggedInUserCommand(cmd: LoggedInUser.LoggedInUserCommand) extends Command

  case object RegisterAnonymousUser extends Command

  sealed trait UsersEvent

  case class UserRegistered(id: User.ID) extends UsersEvent

  private def run(users: Users)(implicit t: ApplicationTime): Behavior[Command] = Behaviors.receive {
    (ctx, message) =>
      message match {
        case RegisterUser(name, email, password) =>
          val id = User.ID.generate()
          val user = LoggedInUser(id, name, email, password)
          val userRef = ctx.spawn(user, LoggedInUser.name(id))
          val evt = UserRegistered(id)
          ctx.system.eventStream.tell(EventStream.Publish(evt))
          run(users.registerUser(id, userRef))
        case WrappedLoggedInUserCommand(command) =>
          users.loggedInUsers.get(command.id) match {
            case Some(user) => user.tell(command)
            case None => command.sender ! UserCommandResponse.failure(command.id, LoggedInUser.NotFoundUser)
          }
          Behaviors.same
        case RegisterAnonymousUser =>
          val id = User.ID.generate()
          val user = AnonymousUser(id)
          run(users.addAnonymousUser(user))
      }
  }
}