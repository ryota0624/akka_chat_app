package com.ryota0624.clean_architecture

trait InputPort[Ctx, I] {
  def input(i: I)(implicit ctx: Ctx)
}

trait OutputPort[Ctx, O] {
  def out(o: O)(implicit ctx: Ctx)
}

trait UseCase[Ctx, I, O] extends InputPort[Ctx, I] with OutputPort[Ctx, O] {
  def execute(i: I)(implicit ctx: Ctx): O

  override def input(i: I)(implicit ctx: Ctx): Unit = {
    val output = execute(i)
    out(output)
  }
}
