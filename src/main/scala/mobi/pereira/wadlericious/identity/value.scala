package mobi.pereira.wadlericious.identity

import mobi.pereira.wadlericious.ast._

package object value {

  sealed trait Value {

    def show: String

    def add(other: Value): Value = Wrong

    def apply(other: Value): IMonad[Value] = new IMonad(Wrong)
  }

  case object Wrong extends Value {

    override def show = "<wrong>"
  }

  case class Number(n: Int) extends Value {

    override def show = String.valueOf(n)

    override def add(other: Value): Value = other match {
      case Number(m) => Number(n + m)
      case _ => Wrong
    }
  }

  case class Function(f: Value => IMonad[Value]) extends Value {

    override def show = "<function>"

    override def apply(other: Value): IMonad[Value] = f(other)
  }

  type Environment = List[(Name, Value)]

  def evaluate(exp: Expression, environment: Environment): IMonad[Value] = exp match {

    case Variable(x) => environment.dropWhile(_._1 != x) match {
      case Nil => new IMonad(Wrong)
      case head :: tail => new IMonad(head._2)
    }

    case Constant(n) => new IMonad(Number(n))

    case Addition(exp1, exp2) => evaluate(exp1, environment).flatMap(e1 =>
      evaluate(exp2, environment).map(e2 =>
        e1.add(e2)
      )
    )

    case Lambda(name, exp) => new IMonad(Function(x => evaluate(exp, (name, x) :: environment)))

    case Application(exp1, exp2) => for {
      e1 <- evaluate(exp1, environment)
      e2 <- evaluate(exp2, environment)
      r <- e1.apply(e2)
    } yield r

    case At(pos, exp) => evaluate(exp, environment) // ignore for now
  }

}