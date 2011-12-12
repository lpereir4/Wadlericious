package mobi.pereira.justplayin.error

import mobi.pereira.justplayin.Monad

trait Maybe[+A] extends Monad[A, Maybe]

case class Failure(msg: String) extends Maybe[Nothing] {

  def map[B](a2b: Nothing => B) = this

  def flatMap[B](a2Mb: Nothing => Maybe[B]) = this
}

case class Success[A](a: A) extends Maybe[A] {

  def map[B](a2b: A => B): Success[B] = Success(a2b(a))

  def flatMap[B](a2Mb: (A) => Maybe[B]) = a2Mb(a)
}