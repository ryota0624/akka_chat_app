package com.ryota0624

import java.time.LocalDateTime

import scala.concurrent.{ExecutionContext, Future}

trait Encrypted[Decrypted] {
  def decrypt(): Decrypted
}

trait CanBeEncrypted {
  protected def toPlainText: String
}

trait CryptoService {
  def crypto[I: CanBeEncrypted, O: Encrypted[I]](ctx: ExecutionContext, value: I): Future[O]
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
