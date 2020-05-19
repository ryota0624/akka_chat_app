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
