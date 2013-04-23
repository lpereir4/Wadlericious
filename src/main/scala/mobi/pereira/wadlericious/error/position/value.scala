package mobi.pereira.wadlericious.error.position

import mobi.pereira.wadlericious.ast._
import mobi.pereira.wadlericious.error.{Success, Failure}

package object value {

  sealed trait Value {

    def show: String

    def add(other: Value): P[Value] = P((p: Position) => Failure("'%s' should be a number (Expression %s)" format(show, p)))

    def apply(other: Value): P[Value] = P((p: Position) => Failure("'%s' should be a function  (Expression %s)" format(show, p)))
  }

  case class Number(n: Int) extends Value {

    override def show = String.valueOf(n)

    override def add(other: Value): P[Value] = other match {
      case Number(m) => P((p: Position) => Success(Number(n + m)))
      case _ => P((p: Position) => Failure("'%s' should be a number (Expression %s)" format(show, p)))
    }
  }

  case class Function(f: Value => P[Value]) extends Value {

    override def show = String.valueOf(f)

    override def apply(other: Value): P[Value] = f(other)
  }

  type Environment = List[(Name, Value)]

  def evaluate(exp: Expression, environment: Environment): P[Value] = exp match {

    case Variable(x) => environment.dropWhile(_._1 != x) match {
      case Nil => P((p: Position) => Failure("'%s' is an unbound variable (Expression %s)" format(x, p)))
      case head :: tail => P((p: Position) => Success(head._2))
    }

    case Constant(n) => P((p: Position) => Success(Number(n)))

    case Addition(exp1, exp2) => for {
      e1 <- evaluate(exp1, environment)
      e2 <- evaluate(exp2, environment)
      r <- e1.add(e2)
    } yield r

    case Lambda(name, exp) => P((p: Position) => Success(Function(x => evaluate(exp, (name, x) :: environment))))

    case Application(exp1, exp2) => for {
      e1 <- evaluate(exp1, environment)
      e2 <- evaluate(exp2, environment)
      r <- e1.apply(e2)
    } yield r

    case At(pos, exp) => P((p: Position) => evaluate(exp, environment).v(pos))
  }

}
