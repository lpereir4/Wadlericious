package mobi.pereira.justplayin

trait Monad[+A, M[_]] {

  def map[B](a2b: A => B): M[B]

  def flatMap[B](a2Mb: A => M[B]): M[B]
}
