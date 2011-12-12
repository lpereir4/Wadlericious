package mobi.pereira.justplayin.identity

import mobi.pereira.justplayin.Monad

/**
 * Identity monad
 */
case class IMonad[+A](a: A) extends Monad[A, IMonad] {

  def map[B](a2b: (A) => B): IMonad[B] = IMonad(a2b(a))

  def flatMap[B](a2Mb: (A) => IMonad[B]): IMonad[B] = a2Mb(a)

  def get: A = a
}