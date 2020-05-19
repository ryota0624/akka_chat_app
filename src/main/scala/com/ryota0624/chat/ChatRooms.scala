package com.ryota0624.chat

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.ryota0624.user.User

object ChatRooms {

  sealed trait Command

  case class CreateChatRoom(ownerID: User.ID, title: ChatRoom.Title) extends Command

  case class WrappedChatRoomCommand(command: ChatRoom.Command) extends Command

  sealed trait Event

  case class ChatRoomCreated(id: ChatRoom.ID) extends Event

  def apply(chatRooms: Map[ChatRoom.ID, ActorRef[ChatRoom.Command]]): Behavior[Command] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case CreateChatRoom(ownerID, title) =>
        val roomID = ChatRoom.ID()
        val chatRoom = ChatRoom(roomID, ownerID, title)
        val chatRoomRef = context.spawn(chatRoom, ChatRoom.name(roomID))
        context.system.eventStream.tell(EventStream.Publish(ChatRoomCreated(roomID)))
        apply(chatRooms + ((roomID, chatRoomRef)))
      case WrappedChatRoomCommand(command) =>
        chatRooms.get(command.chatRoomID) match {
          case Some(roomRef) => roomRef ! command
            Behaviors.same
          case None =>
            // TODO 本当は送りつけてきた側にNotFoundしたいけど面倒なので一旦スルー
            Behaviors.same
        }
    }
  }
}
