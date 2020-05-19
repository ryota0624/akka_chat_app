package com.ryota0624.application

import akka.actor.typed.eventstream.EventStream.Subscribe
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.ryota0624.chat.ChatRooms
import com.ryota0624.user.{LoggedInUser, User, Users}
import com.ryota0624.{ApplicationTime, user}


object ChatApplication {

  sealed trait Command

  final case class UsersCommand(command: Users.Command) extends Command

  final case class ChatRoomsCommand(command: ChatRooms.Command) extends Command

  def apply()(implicit time: ApplicationTime): Behavior[Command] = Behaviors.setup { context =>
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

    }
  }
}

object CommandLine extends App {
  implicit val applicationTime: ApplicationTime = ApplicationTime()
  val main: ActorSystem[ChatApplication.Command] = ActorSystem(ChatApplication(), "ChatApplication")

  main ! ChatApplication.UsersCommand(Users.RegisterAnonymousUser)
  main ! ChatApplication.UsersCommand(Users.RegisterAnonymousUser)
  main ! ChatApplication.UsersCommand(Users.RegisterUser(
    User.Name("suzuki"),
    new LoggedInUser.Email("hoge"),
    new user.LoggedInUser.Password("huga"))
  )
}
