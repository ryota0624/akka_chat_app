package com.ryota0624.user

import com.ryota0624.{CanBeEncrypted, Encrypted}

import scala.concurrent.ExecutionContext


// Userはchat appの利用者です。
trait User {

  import com.ryota0624.user.User._

  def id: ID

  def name: Name
}

object User {

  class ID(private val value: String)

  object ID {
    def apply(value: String): User.ID = new ID(value)

    def generate(): ID = ID(java.util.UUID.randomUUID().toString)
  }


  class Name(private val name: String)

  object Name {
    def apply(name: String): Name = new Name(name)
  }

}


// LoggedInUser はログインした利用者です。
class LoggedInUser(
                    val id: User.ID,
                    val name: User.Name,
                    val email: LoggedInUser.Email,
                    val encryptedPassword: LoggedInUser.EncryptedPassword,
                  ) extends User

object LoggedInUser {

  class Email(private val value: String)

  class Password(private val plainText: String) extends CanBeEncrypted {
    override def toPlainText: String = plainText
  }

  class EncryptedPassword(private val value: String) extends Encrypted[Password] {
    override def decrypt(ctx: ExecutionContext): Password = ???
  }

}

// AnonymousUser はログインしていない利用者です。
class AnonymousUser(
                     val id: User.ID,
                     val name: User.Name,
                   ) extends User

object AnonymousUser {

  def apply(): AnonymousUser = new AnonymousUser(User.ID.generate(), generateRandomName())

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