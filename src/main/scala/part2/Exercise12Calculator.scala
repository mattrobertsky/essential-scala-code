package part2

// ----------------------------------------------

// Step 1. Write a definition for Expr here!

sealed trait Expr {
  def stringify: String
  val result: Double
}

// Handle the following types of equation:
// - addition

case class Addition(
  a: Double,
  b: Double
) extends Expr {
  val result = a + b
  override def stringify: String = s"$a + $b = $result"
}

// - subtraction
case class Subtraction(
  a: Double,
  b: Double
) extends Expr {
  override val result: Double = a - b
  override def stringify: String = s"$a - $b = $result"
}

// - multiplication
case class Multiplication(
  a: Double,
  b: Double
) extends Expr {
  override val result: Double = a * b
  override def stringify: String = s"$a * $b = $result"
}

// - division
case class Division(
  a: Double,
  b: Double
) extends Expr {
  override val result: Double = a / b
  override def stringify: String = s"$a / $b = $result"
}

// - square root
case class SquareRoot(
  a: Double
) extends Expr {
  override val result: Double = scala.math.sqrt(a)
  override def stringify: String = s"sqrRoot $a = $result"
}

// Give it a `stringify` method
// that renders the expression as a string.

// ----------------------------------------------

// Step 2. Implement eval
// for each of the "calculator" objects below:

object Calculator {
  def eval(calc: Expr): Double = {
    calc.result
  }

//  def evalTwo(calc: Expr): Double = calc match {
//    case SquareRoot(a) => scala.math.sqrt(a) // etc
//  }

}

object IntCalculator {
  def eval(calc: Expr): Int = calc match {
    case SquareRoot(a) => scala.math.sqrt(a).toInt
    case Multiplication(a, b) => (a * b).toInt
    case Subtraction(a, b) => (a - b).toInt
    case Division(a, b) => (a / b).toInt
    case Addition(a, b) => (a + b).toInt
  }
}

object SafeIntCalculator {
  def eval(calc: Expr): Either[String, Int] = calc match {
    case Division(_, 0) => Left("Error, division by Zero")
    case SquareRoot(a) => Right(scala.math.sqrt(a).toInt)
    case Multiplication(a, b) => Right((a * b).toInt)
    case Subtraction(a, b) => Right((a - b).toInt)
    case Division(a, b) => Right((a/b).toInt)
    case Addition(a, b) => Right((a + b).toInt)
  }
}



// ----------------------------------------------

// Step 3. Write some convenience methods
// for constructing common calculations:

// ----------------------------------------------

trait CalcHelpers {
  implicit class BetterDouble(someDouble: Double) {
    def ~+(otherDouble: Double): Expr = Addition(someDouble, otherDouble)
    def ~+(expression: Expr): Expr = Addition(someDouble, expression.result)

    def ~-(otherDouble: Double): Expr = Subtraction(someDouble, otherDouble)
    def ~-(expression: Expr): Expr = Subtraction(someDouble, expression.result)

    def ~*(otherDouble: Double): Expr = Multiplication(someDouble, otherDouble)
    def ~*(expression: Expr): Expr = Multiplication(someDouble, expression.result)

    def ~/(otherDouble: Double): Expr = Division(someDouble, otherDouble)
    def ~/(expression: Expr): Expr = Division(someDouble, expression.result)

    def sqrt: Expr = SquareRoot(someDouble)
  }
}
object Expr {
  type Num = Double

   def pythag(a: Double, b: Double): Expr = {
     SquareRoot(
       Addition(
         Multiplication(a, a).result,
         part2.Multiplication(b, b).result
       ).result
     )
   }

   def factorial(n: Int): Expr = {
     if(n < 1)
       Multiplication(1,1)
     else
       Multiplication(n, factorial(n - 1).result)
   }
}


object Exercise11Calculator extends CalcHelpers {

  val calc1: Expr = 1.1 ~+ (2.2 ~* 3.3)
  val calc2 = 3.3 ~+ (1.1 ~* 2.2)

  val calc3 = 3.3 ~/ 0

  def main(args: Array[String]): Unit = {
    println("stringify")
    println(calc1.stringify)
    println(calc2.stringify)

    println("Calculator.eval")
    println(Calculator.eval(calc1))
    println(Calculator.eval(calc2))
    println(Calculator.eval(calc3))

    println("IntCalculator.eval")
    println(IntCalculator.eval(calc1))
    println(IntCalculator.eval(calc2))
    println(IntCalculator.eval(calc3))

    println("SafeIntCalculator.eval")
    println(SafeIntCalculator.eval(calc1))
    println(SafeIntCalculator.eval(calc2))
    println(SafeIntCalculator.eval(calc3))

    println("pythag")
    println(Expr.pythag(3, 4))
    println(Calculator.eval(Expr.pythag(3, 4)))
    println(IntCalculator.eval(Expr.pythag(3, 4)))

    println("factorial")
    println(Expr.factorial(4))
    println(Calculator.eval(Expr.factorial(4)))
    println(IntCalculator.eval(Expr.factorial(4)))
  }
}
