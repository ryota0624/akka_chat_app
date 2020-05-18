package com.ryota0624.chat

import com.ryota0624.user.User

// ChatRoom はChatをするための空間です。
trait ChatRoom {

  import com.ryota0624.chat.ChatRoom._

  def id: ID

  def ownerID: User.ID

  def title: Title

  def maxUserCount: Int

  def textCount: Int

  def startConversation(text: Text): Conversation
}

object ChatRoom {
  private val maxTextCount = 1000

  class Title(private val value: String)

  class ID(private val value: String)

}

// Conversation は会話です。
trait Conversation {

  import Conversation._

  def id: ID

  def roomID: ChatRoom.ID

  def ownerID: User.ID

  def participants: Seq[User.ID]

  def root: Text

  def speak(text: Text): Conversation
}

object Conversation {

  class ID(private val value: String)

}

trait Text {

  import Text._

  def id: ID

  def typedFrom: User.ID

  def parentConversationID: ParentConversationID

  def parentTextID: Option[ID]

  def letter: String

  def mentionedTo: Seq[User.ID]

  def links: Seq[Link]
}

object Text {

  class ParentConversationID(private val conversationID: Conversation.ID)

  class ID(private val value: String)

  class Link(private val value: String)

}