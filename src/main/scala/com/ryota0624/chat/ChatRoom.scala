package com.ryota0624.chat

import com.ryota0624.user.User

// ChatRoom は会話をするための空間です。
trait ChatRoom {

  import com.ryota0624.chat.ChatRoom._

  def id: ID

  def ownerID: User.ID

  def title: Title

  def maxUserCount: Int

  def startConversation(text: Text): Conversation

  def join(user: User): Conversation

  def joined: Seq[User.ID]
}

object ChatRoom {
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

// Text は会話中の発言です。
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