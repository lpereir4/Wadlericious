package mobi.pereira.justplayin.error

import mobi.pereira.justplayin.ast._

package object value {

  sealed trait Value {

    def show: String

    def add(other: Value): Maybe[Value] = Failure("Should be a number: " + show)

    def apply(other: Value): Maybe[Value] = Failure("Should be a function: " + show)
  }

  case class Number(n: Int) extends Value {

    override def show = String.valueOf(n)

    override def add(other: Value): Maybe[Value] = other match {
      case Number(m) => Success(Number(n + m))
      case _ => Failure("Should be a number: " + show)
    }
  }

  case class Function(f: Value => Maybe[Value]) extends Value {

    override def show = "<function>"

    override def apply(other: Value): Maybe[Value] = f(other)
  }

  type Environment = List[(Name, Value)]

  def evaluate(exp: Expression, environment: Environment): Maybe[Value] = exp match {

    case Variable(x) => environment.dropWhile(_._1 != x) match {
      case Nil => Failure("Unbound variable: " + x)
      case head :: tail => Success(head._2)
    }

    case Constant(n) => Success(Number(n))

    case Addition(exp1, exp2) => evaluate(exp1, environment).flatMap(e1 =>
      evaluate(exp2, environment).flatMap(e2 =>
        e1.add(e2)
      )
    )

    case Lambda(name, exp) => Success(Function(x => evaluate(exp, (name, x) :: environment)))

    case Application(exp1, exp2) => for {
      e1 <- evaluate(exp1, environment)
      e2 <- evaluate(exp2, environment)
      r <- e1.apply(e2)
    } yield r

    case At(pos, exp) => evaluate(exp, environment) // ignore for now
  }

}