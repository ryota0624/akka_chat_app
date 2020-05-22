package com.ryota0624.application

import akka.actor.typed.eventstream.EventStream.Subscribe
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.ryota0624.chat.ChatRooms
import com.ryota0624.user.LoggedInUser.Email
import com.ryota0624.user.{LoggedInUser, User, Users}
import com.ryota0624.{
  ApplicationTime,
  ReplayDocument,
  ReplayableCommand,
  Response,
  ValidationError,
  user
}

import scala.util.{Failure, Success, Try}

object QueryMaterializer {
  sealed trait Command[D <: ReplayDocument] extends ReplayableCommand[D]

  final case class GetLoggedInUsers(
      replayTo: ActorRef[Response[GetLoggedInUsers#Document]],
  ) extends Command[GetLoggedInUsers#Document] {
    final case class Document(users: Seq[LoggedInUserView])
        extends ReplayDocument
  }

  case class AnonymousUsersView(ids: Seq[User.ID]) {
    def add(id: User.ID): AnonymousUsersView = copy(ids = ids :+ id)
  }

  case class LoggedInUserView(
      id: User.ID,
      name: User.Name,
      email: Email
  )

  case class LoggedInUsersView(users: Seq[LoggedInUserView]) {
    def add(u: LoggedInUserView): LoggedInUsersView = copy(users = users :+ u)
  }

  def handleCommand(anonymousUsers: AnonymousUsersView,
                    loggedInUsers: LoggedInUsersView,
                    command: ReplayableCommand[_])
    : Either[ValidationError, ReplayDocument] = {
    command match {
      case cmd: GetLoggedInUsers =>
        Right(cmd.Document(loggedInUsers.users))
    }
  }

  def start(
      anonymousUsers: AnonymousUsersView,
      loggedInUsers: LoggedInUsersView
  ): Behavior[Any] = Behaviors.setup { subCtx =>
    subCtx.system.eventStream.tell(Subscribe(subCtx.self))
    Behaviors.receiveMessage {
      case Users.AnonymousUserRegistered(id) =>
        start(anonymousUsers.add(id), loggedInUsers)
      case activated: LoggedInUser.Activated =>
        start(
          anonymousUsers,
          loggedInUsers.add(
            LoggedInUserView(activated.id, activated.name, activated.email)))
      case cmd: ReplayableCommand[_] =>
        val document = handleCommand(anonymousUsers, loggedInUsers, cmd)
        document match {
          case Left(err) =>
            cmd.replayTo ! Response.Failure(err)
          case Right(value) =>
            cmd.replayTo ! Response.Success(value)
        }
        Behaviors.same
      case _ =>
        Behaviors.ignore
    }
  }
}

object ChatApplication {

  sealed trait Command

  final case class UsersCommand(command: Users.Command) extends Command

  final case class ChatRoomsCommand(command: ChatRooms.Command) extends Command

  final case class UsersResponse(msg: Response[Users.Document]) extends Command

  def apply()(implicit time: ApplicationTime): Behavior[Command] =
    Behaviors.setup { context =>
      val usersResponseAdapter = context.messageAdapter(UsersResponse)
      context.self ! ChatApplication.UsersCommand(
        Users.RegisterAnonymousUser(usersResponseAdapter))
      context.self ! ChatApplication.UsersCommand(
        Users.RegisterAnonymousUser(usersResponseAdapter))
      context.self ! ChatApplication.UsersCommand(
        Users.RegisterUser(
          User.Name("suzuki"),
          new LoggedInUser.Email("hoge"),
          new user.LoggedInUser.Password("huga"),
          usersResponseAdapter
        ))

      def startSubscriber(): Behavior[Any] = Behaviors.setup { subCtx =>
        subCtx.system.eventStream.tell(Subscribe(subCtx.self))
        Behaviors.receiveMessage { message =>
          subCtx.log.info(s"received message $message")
          Behaviors.same
        }
      }

      val _ = context.spawn(startSubscriber(), "subscriber")
      val chatRooms = context.spawn(ChatRooms(Map.empty), "chat_rooms")
      val users = context.spawn(Users.apply(), "users")
      Behaviors.receiveMessage {
        case UsersCommand(command) =>
          users ! command
          Behaviors.same
        case ChatRoomsCommand(command) =>
          chatRooms ! command
          Behaviors.same
        case UsersResponse(msg) =>
          context.log.info(msg.toString)
          Behaviors.same
      }
    }
}

object CommandLine extends App {
  implicit val applicationTime: ApplicationTime = ApplicationTime()
  val main: ActorSystem[ChatApplication.Command] =
    ActorSystem(ChatApplication(), "ChatApplication")
}
