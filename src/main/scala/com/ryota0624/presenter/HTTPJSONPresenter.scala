package com.ryota0624.presenter

import com.ryota0624.clean_architecture.OutputPort

trait HTTPJSONPresenter[Ctx, O]
    extends OutputPort[Ctx, O]
    with RequestProvider {
  override def out(o: O)(implicit ctx: Ctx): Unit = ???
}

trait RequestProvider {
  def provide: Any
}
