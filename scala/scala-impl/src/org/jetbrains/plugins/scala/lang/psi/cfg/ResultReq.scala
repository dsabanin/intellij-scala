package org.jetbrains.plugins.scala.lang.psi.cfg

sealed abstract class ResultReq(final val needed: Boolean) {
  type Result[T] >: Some[T] <: Option[T]

  def ifNeeded[T](f: => T): Result[T]
  final def orIfNeeded[T](opt: Option[T], defaultIfNeeded: => T): Result[T] = opt match {
    case some@Some(_) => some
    case None => ifNeeded(defaultIfNeeded)
  }

  @inline
  def map[T, TT](opt: Result[T])(f: T => TT): Result[TT]
}

object ResultReq {
  final case object Needed extends ResultReq(true) {
    override type Result[T] = Some[T]

    override def ifNeeded[T](f: => T): Result[T] = Some(f)
    override def map[T, TT](opt: Result[T])(f: T => TT): Some[TT] = Some(f(opt.value))
  }

  final case object NotNeeded extends ResultReq(false) {
    override type Result[T] = Option[T]

    override def ifNeeded[T](f: => T): Result[T] = None
    override def map[T, TT](opt: Result[T])(f: T => TT): Result[TT] = opt.map(f)
  }
}