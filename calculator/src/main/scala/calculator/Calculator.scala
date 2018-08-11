package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  /*
  This function takes as input a map from variable name to expressions of their values.
  Since the expression is derived from the text entered by the user, it is a Signal.
  The Expr abstract data type is defined as follows

  The Ref(name) case class represents a reference to another variable in the map namedExpressions.
  The other kinds of expressions have the obvious meaning

  The function should return another map from the same set of variable names to their actual values,
  computed from their expressions.
   */
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val t = for {
      (k, v) <- namedExpressions
    } yield { (k -> Signal(eval(v(), namedExpressions))) }
    t
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => {
        val ref = getReferenceExpr(name, references)
        eval(ref, references - name)
      }
      case Plus(a, b)   => eval(a, references) + eval(b, references)
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
