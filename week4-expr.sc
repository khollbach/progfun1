object Main {
  def main(args: Array[String]) = {
    val sum = Sum(Number(1), Number(2))
    println(show(sum))
    val prod = Product(sum, Var("x"))
    println(show(prod))
  }

  /*def show(e: Expr): String =*/
    /*e match {*/
      /*case Number(n) => n.toString*/
      /*case Sum(e1, e2) => show(e1) + " + " + show(e2)*/
    /*}*/

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Var(name) => name
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Product(e1, e2) => {
      def parenShow(expr: Expr): String =
        expr match {
          case Sum(_, _) => "(" + show(expr) + ")"
          case _ => show(expr)
        }
      parenShow(e1) + " * " + parenShow(e2)
    }
  }
}

trait Expr

case class Number(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Product(e1: Expr, e2: Expr) extends Expr
