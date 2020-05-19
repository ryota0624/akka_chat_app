package com.ryota0624.application

import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.ryota0624.ApplicationTime
import com.ryota0624.chat.ChatRooms
import com.ryota0624.user.Users


object ChatApplication {

  sealed trait Command
  final case class UsersCommand(command: Users.Command)extends Command
  final case class ChatRoomsCommand(command: ChatRooms.Command) extends Command

  def apply()(implicit time: ApplicationTime): Behavior[Command] = Behaviors.setup { context =>
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

class CommandLine extends App {
  implicit val applicationTime: ApplicationTime = ApplicationTime()
  val main: ActorSystem[ChatApplication.Command] = ActorSystem(ChatApplication(), "ChatApplication")
}
