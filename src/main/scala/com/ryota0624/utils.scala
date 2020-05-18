package com.ryota0624

import scala.concurrent.{ExecutionContext, Future}

trait Encrypted[Decrypted] {
  def decrypt(ctx: ExecutionContext): Decrypted
}

trait CanBeEncrypted {
  def toPlainText: String
}

trait CryptoService {
  def crypto[I: CanBeEncrypted, O: Encrypted[I]](ctx: ExecutionContext, value: I): Future[O]
}



