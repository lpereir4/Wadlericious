package mobi.pereira.justplayin

import ast._
import error.position.P
import error.position.value.{Value, evaluate}

object Main {

  def main(args: Array[String]): Unit = {
    val success1 = Application(Lambda("x", Addition(At(Position(2), Variable("a")), Variable("x"))), Addition(Constant(10), Constant(11)))

    val success2 = Application(Lambda("x", At(Position(9), Addition(At(Position(6), Variable("a")), Variable("x")))), Addition(Constant(10), Constant(11)))

    val success3 = Application(Lambda("x", Addition(Variable("x"), Variable("x"))), Addition(Variable("x"), Constant(11)))

    val success4 = Application(Lambda("x", Addition(Variable("x"), Variable("x"))), Addition(Constant(10), Constant(11)))

    val failure = Application(Constant(1), Constant(2))

    val e1: P[Value] = evaluate(failure, Nil)
    println(e1.v(Position(-1)))
    val e2: P[Value] = evaluate(success1, Nil)
    println(e2.v(Position(-1)))
    val e3: P[Value] = evaluate(success2, Nil)
    println(e3.v(Position(-1)))
    val e4: P[Value] = evaluate(success3, Nil)
    println(e4.v(Position(-1)))
    val e5: P[Value] = evaluate(success4, Nil)
    println(e5.v(Position(-1)))
  }
}
