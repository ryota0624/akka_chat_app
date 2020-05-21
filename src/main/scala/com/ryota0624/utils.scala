package com.ryota0624

import java.time.LocalDateTime

import akka.actor.typed.ActorRef

import scala.concurrent.{ExecutionContext, Future}

trait Encrypted[Decrypted] {
  def decrypt(): Decrypted
}

trait CanBeEncrypted {
  protected def toPlainText: String
}

trait CryptoService {
  def crypto(ctx: ExecutionContext, value: CanBeEncrypted): Future[Encrypted[_]]
}

trait ApplicationTime {
  def now(): LocalDateTime
}

object ApplicationTimeImpl extends ApplicationTime {
  def now(): LocalDateTime = LocalDateTime.now()
}

object ApplicationTime {
  def apply(): ApplicationTime = ApplicationTimeImpl
}

trait ReplayableCommand[D >: ReplayDocument] {
  def replayTo: ActorRef[Response[D]]
}

trait ValidationError

trait ReplayDocument

sealed trait Response[D >: ReplayDocument]

object Response {
  final case class Failure(error: ValidationError) extends Response[Any]
  final case class Success[T >: ReplayDocument](document: T) extends Response[T]
}
