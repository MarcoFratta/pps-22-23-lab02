package lab


import scala.annotation.tailrec

object Main extends App {

  import math.*

  // TASK 3A svolto da solo
  println("-- TASK 3A --")
  val positive: Int => String = _ match
    case x if x >= 0 => "positive"
    case _ => "negative"

  def positiveMethod(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  val k = 3
  println(f"is $k positive?  ${positiveMethod(k)}")

  //TASK 3B  svolto da solo
  println("-- TASK 3B --")

  var neg: (String => Boolean) => String => Boolean = f => x => !f(x)

  def negMethod(f: String => Boolean): String => Boolean = x => !f(x)


  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = negMethod(empty) // which type of notEmpty?
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

  println("-- TASK 3C --")

  def genericNeg[T](f: T => Boolean): T => Boolean = x => !f(x)

  val n = "Fox"
  println(f"is '$n' not empty?  ${genericNeg(empty)(n)}")
  println(f"is '' not empty?  ${genericNeg(empty)("")}")

  // TASK 4  svolto da solo
  println("-- TASK 4 --")
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  val x = 3
  val y = 4
  val z = 4
  println(f"is $x <= $y = $z? ${p3(x)(y)(z)}")

  // TASK 5  svolto da solo
  println("-- TASK 5 --")

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  println(compose(_ - 1, _ * 2)(5)) //9

  def genericCompose[X, Y, Z](f: Y => Z, g: X => Y): X => Z = x => f(g(x))

  println(genericCompose(empty, positive)(6))

  // TASK 6  svolto da solo
  println("-- TASK 6 --")

  @tailrec
  def gcd(a: Int, b: Int): Int = (max(a, b), min(a, b)) match
    case (a, 0) => a
    case (a, b) => gcd(b, a % b)


  println(f"GCD 12,8 =  ${gcd(12, 8)}  ") // 4
  println(f"GCD 7,14 =  ${gcd(7, 14)}  ") // 7
  println(f"GCD 3,0 =  ${gcd(3, 0)}  ") // 3
  println(f"GCD 3,1 =  ${gcd(3, 1)}  ") // 1


  // TASK 7  svolto da solo
  println("-- TASK 7 --")

  enum Shape:
    case Square(l: Double, bottomLeft: (Double, Double))
    case Circle(r: Double, center: (Double, Double))
    case Rectangle(width: Double, height: Double, bottomLeft: (Double, Double))


  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Square(l, _) => l * 4
      case Circle(r, _) => 2 * r * math.Pi
      case Rectangle(hx, hy, _) => 2 * hx + 2 * hy

    private def inRange(x: Double, a: Double, b: Double): Boolean = x >= a && x <= b

    private def pow2(x: Double): Double = x * x



    def contains(shape: Shape, p: (Double, Double)): Boolean = shape match
      case Square(l, lf) => inRange(p._1, lf._1, lf._1 + l) && inRange(p._2, lf._2, lf._2 + l)
      case Circle(r, c) => sqrt(pow2(p._1 - c._1) + pow2(p._2 - c._2)) <= r
      case Rectangle(hx, hy, lf) => inRange(p._1, lf._1, lf._1 + hx) && inRange(p._2, lf._2, lf._2 + hy)


  import Shape.*


  val square = Shape.Square(5, (0, 0))
  val circle = Shape.Circle(1, (0, 0))
  val rectangle = Shape.Rectangle(3, 5, (0, 0))

  println(f"5 x 5 square perimeter is > ${Shape.perimeter(square)} ") // 20
  println(f"Radius 1 circle perimeter is > ${Shape.perimeter(circle)} ") // 6.28..
  println(f"3x5 rectangle perimeter is > ${Shape.perimeter(rectangle)} ") // 16

  println(f"(3,3) is inside 5 x 5 square > ${Shape.contains(square, (3, 3))} ") // true
  println(f"(-1,6) is inside 5 x 5 square > ${Shape.contains(square, (-1, 6))} ") // false

  println(f"(1,0) is inside 5 x 5 circle > ${Shape.contains(circle, (1, 0))} ") // true
  println(f"(-1,6) is inside 5 x 5 circle > ${Shape.contains(circle, (-1, 6))} ") // false

  println(f"(1,0) is inside 3 x 5 rectangle > ${Shape.contains(circle, (1, 0))} ") // true
  println(f"(-1,6) is inside 3 x 5 rectangle > ${Shape.contains(circle, (-1, 6))} ") // false


  // TASK 8  svolto da solo
  println("-- TASK 8 --")

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[X](x: Option[X])(f: X => Boolean): Option[X] = x match
      case Some(a) if f(a) => x
      case _ => None()

    def map[A, B](x: Option[A])(f: A => B): Option[B] = x match
      case Some(a) => Some(f(a))
      case _ => None()

    def fold[A, B](x: Option[A])(default: B)(f: A => B): B = x match
      case Some(a) => f(a)
      case _ => default

  import Option.*

  println("Filter:")
  println(f"filter(Some(5)(_ > 2)) = ${filter(Some(5))(_ > 2)}") // Some(5)
  println(f"filter(Some(5))(_ k> 8)) = ${filter(Some(5))(_ > 8)}") // None
  println(f"filter(None[Int]())(_ > 2) = ${filter(None[Int]())(_ > 2)}") // None
  println("Map:")
  println(f"map(Some(5))(_ > 2) = ${map(Some(5))(_ > 2)}") // Some(true)
  println(f"map(Some(5))(_ > 8) = ${map(Some(5))(_ > 8)}") // Some(false)
  println(f"map(None[Int]())(_ > 2) = ${map(None[Int]())(_ > 2)}") // None
  println(f"map(Some(4))(positive) = ${map(Some(4))(positive)}") // Some(positive)
  println("Fold:")
  println(f"fold(Some(5))(1)(_ + 1) =  ${fold(Some(5))(1)(_ + 1)}") // 6
  println(f"fold(None[Int]())(1)(_ + 1) =  ${fold(None[Int]())(1)(_ + 1)}") // 1


}