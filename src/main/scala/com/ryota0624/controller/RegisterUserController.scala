package com.ryota0624.controller

import com.ryota0624.clean_architecture.InputPort
import com.ryota0624.usecase
import com.ryota0624.presenter
import com.ryota0624.presenter.RequestProvider

class RegisterUserController(
    private val inputPort: InputPort[usecase.Ctx, usecase.RegisterUser.Input]
) {
  def handle(request: Any): Unit = {
    inputPort.input(???)(???)
  }
}

class Router(
    registerUser: RegisterUserController
) {
  def handle(request: Any) = {
    trait RequestProviderImpl extends RequestProvider {
      override def provide: Any = request
    }

    // TODO content type見てpresenter変える
    new RegisterUserController(
      new usecase.RegisterUser
      with presenter.HTTPJSONPresenter[usecase.Ctx, usecase.RegisterUser.Output]
      with RequestProviderImpl
    ).handle(request)
  }
}
