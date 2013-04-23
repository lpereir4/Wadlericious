package mobi.pereira.wadlericious


import mobi.pereira.wadlericious.ast.Addition
import mobi.pereira.wadlericious.ast.Application
import mobi.pereira.wadlericious.ast.Constant
import mobi.pereira.wadlericious.ast.Lambda
import mobi.pereira.wadlericious.ast.Position
import mobi.pereira.wadlericious.ast.Variable
import mobi.pereira.wadlericious.error.{Success, Maybe}
import mobi.pereira.wadlericious.error.position.value._
import org.scalatest.FunSuite

class EvaluatorSuite extends FunSuite {

  test("x. x+x") {
    val expression = Application(Lambda("x", Addition(Variable("x"), Variable("x"))), Constant(21))
    val result: Maybe[Value] = evaluate(expression, Nil).v(Position(-1))

    assert(result.isInstanceOf[Success[Value]])
    assert(result.asInstanceOf[Success[Value]].a == Number(42))
  }
}