package mobi.pereira.justplayin.error.position

import mobi.pereira.justplayin.ast.Position
import mobi.pereira.justplayin.Monad
import mobi.pereira.justplayin.error.Maybe

case class P[+A](v: Position => Maybe[A]) extends Monad[A, P] {

  def map[B](a2b: A => B): P[B] = P((p: Position) => v(p).map(a2b))

  def flatMap[B](a2Mb: A => P[B]): P[B] = P((p: Position) => v(p).flatMap((a: A) => a2Mb(a).v(p)))
}
