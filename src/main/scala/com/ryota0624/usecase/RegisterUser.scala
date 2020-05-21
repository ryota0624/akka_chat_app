package com.ryota0624.usecase

import com.ryota0624.clean_architecture.UseCase
import com.ryota0624.user.{LoggedInUser, User}

object RegisterUser {
  final case class Input(
      name: User.Name,
      email: LoggedInUser.Email,
      password: LoggedInUser.Password,
  )

  final case class Output(
      id: User.ID
  )
}

trait RegisterUser
    extends UseCase[Ctx, RegisterUser.Input, RegisterUser.Output] {
  override def execute(i: RegisterUser.Input)(
      implicit ctx: Ctx): RegisterUser.Output = {
    // TODO Actorに放りなげる
    ???
  }
}
