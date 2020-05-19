package com.ryota0624.chat

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.ryota0624.chat.ChatRoom.Title
import com.ryota0624.user.User

// ChatRoom は会話をするための空間です。
case class ChatRoom(
                     id: ChatRoom.ID,
                     ownerID: User.ID,
                     title: Title,
                     joined: Seq[User.ID],
                     conversations: Map[Conversation.ID, ActorRef[Conversation.Command]]
                   ) {
  def startConversation(text: Text)(implicit actorCtx: ActorContext[_]): ChatRoom = {
    val conversationID = Conversation.ID()
    val conversationRef = actorCtx.spawn(Conversation(conversationID, id, text.typedFrom, text), Conversation.name(conversationID))
    copy(conversations = conversations + ((conversationID, conversationRef)))
  }

  def join(id: User.ID): ChatRoom = {
    copy(joined = joined :+ id)
  }
}

object ChatRoom {
  def name(id: ID) = s"chat_room/${id.toString}"

  def apply(id: ID, ownerID: User.ID, title: Title): Behavior[Command] =
    run(new ChatRoom(id, ownerID, title, Seq(ownerID), Map.empty))

  class Title(private val value: String)

  class ID(private val value: String) {
    override def toString: String = value
  }

  object ID {
    def apply(): ID = new ID(java.util.UUID.randomUUID().toString)
  }

  sealed trait Command {
    def chatRoomID: ChatRoom.ID
  }

  sealed trait ValidationError

  case class NotFoundConversation(conversation: Conversation.ID) extends ValidationError

  case class Response(id: ChatRoom.ID, validationError: Option[ValidationError])

  object Response {
    def failure(id: ChatRoom.ID, error: ValidationError): Response = Response(id, Some(error))

    def success(id: ChatRoom.ID): Response = Response(id, None)
  }

  final case class AddText(chatRoomID: ChatRoom.ID, conversationID: Conversation.ID, text: Text, sender: ActorRef[Response]) extends Command

  final case class StartConversation(chatRoomID: ChatRoom.ID, text: Text) extends Command

  final case class WrappedConversationResponse(chatRoomID: ID, response: Conversation.Response) extends Command

  private def run(chatRoom: ChatRoom): Behavior[Command] = {
    Behaviors.receive {
      (actorCtx, message) =>
        if (message.chatRoomID == chatRoom.id) message match {
          case StartConversation(_, text) =>
            val updatedChatRoom = chatRoom.startConversation(text)(actorCtx)
            run(updatedChatRoom)
          case AddText(_, conversationID, text, sender) =>
            chatRoom.conversations.get(conversationID) match {
              case Some(ref) =>
                ref ! Conversation.AddText(conversationID, text)
                Behaviors.same
              case None =>
                sender ! Response.failure(message.chatRoomID, NotFoundConversation(conversationID))
                Behaviors.same
            }
        }
        else Behaviors.same
    }
  }
}

// Conversation は会話です。
case class Conversation(
                         id: Conversation.ID,
                         roomID: ChatRoom.ID,
                         ownerID: User.ID,
                         participants: Set[User.ID],
                         root: Text,
                         childrenTexts: Seq[Text],
                       ) {
  def addText(text: Text): Conversation = {
    val updatedParticipants = participants + text.typedFrom ++ text.mentionedTo
    copy(childrenTexts = childrenTexts :+ text, participants = updatedParticipants)
  }
}

object Conversation {

  def name(id: ID) = s"conversations/${id.toString}"

  sealed trait Command {
    def conversationID: Conversation.ID
  }

  case class Response(id: Conversation.ID, validationError: Option[ValidationError])

  object Response {
    def failure(id: Conversation.ID, error: ValidationError) = Response(id, Some(error))

    def success(id: Conversation.ID) = Response(id, None)
  }

  sealed trait ValidationError

  final case class AddText(conversationID: Conversation.ID, text: Text) extends Command

  class ID(private val value: String) {
    override def toString: String = value
  }

  object ID {
    def apply(): ID = new ID(java.util.UUID.randomUUID().toString)
  }

  def apply(
             id: ID, roomID: ChatRoom.ID, ownerID: User.ID, root: Text
           ): Behavior[Command] = run(new Conversation(id, roomID, ownerID, Set(ownerID), root, childrenTexts = Nil))

  private def run(conversation: Conversation): Behavior[Command] = Behaviors.receive {
    (_, message) =>
      if (conversation.id == message.conversationID) message match {
        case AddText(_, text) =>
          val updated = conversation.addText(text)
          //          // TODO validation
          //          sender ! Response.success(message.conversationID)
          run(updated)
      }

      else Behaviors.same
  }

}

// Text は会話中の発言です。
case class Text(
                 id: Text.ID,
                 typedFrom: User.ID,
                 parentConversationID: Text.ParentConversationID,
                 parentTextID: Option[User.ID],
                 letter: String,
                 mentionedTo: Seq[User.ID],
                 links: Seq[Text.Link],
               )

object Text {

  class ParentConversationID(private val conversationID: Conversation.ID)

  class ID(private val value: String)

  class Link(private val value: String)

}