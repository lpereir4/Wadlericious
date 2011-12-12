package mobi.pereira.justplayin

package object ast {

  type Name = String

  case class Position(p: Int) {
    override def toString: String = p match {
      case -1 => "Unknown"
      case _ => "#"+String.valueOf(p)
    }
  }

  sealed trait Expression

  case class Variable(x: Name) extends Expression

  case class Constant(n: Int) extends Expression

  case class Addition(exp1: Expression, exp2: Expression) extends Expression

  case class Lambda(name: Name, exp: Expression) extends Expression

  case class Application(exp1: Expression, exp2: Expression) extends Expression

  case class At(pos: Position, exp: Expression) extends Expression

}