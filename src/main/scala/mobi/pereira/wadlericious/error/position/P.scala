package mobi.pereira.wadlericious.error.position

import mobi.pereira.wadlericious.ast._
import mobi.pereira.wadlericious.error.Maybe
import mobi.pereira.wadlericious.Monad

case class P[+A](v: Position => Maybe[A]) extends Monad[A, P] {

  def map[B](a2b: A => B): P[B] = P((p: Position) => v(p).map(a2b))

  def flatMap[B](a2Mb: A => P[B]): P[B] = P((p: Position) => v(p).flatMap((a: A) => a2Mb(a).v(p)))
}
