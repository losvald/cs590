package cs590.week4

import scala.virtualization.lms.common._

import scala.language.postfixOps // avoid warning about unzip
import scala.language.implicitConversions

trait Power1 extends ScalaOpsPkg with LiftNumeric {

  def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0 else b * power(b, x - 1)

}

trait Power2 extends ScalaOpsPkg with LiftNumeric {

  def power(b: Rep[Double], x: Int): Rep[Double] = {
    if (x == 0) 1.0
    else if (x % 2 != 0) b * power(b, x - 1)
    // else power(b, x / 2) * power(b, x / 2) // OK because of tail rec opt.
    else { // don't rely on tail recursion optimization
      val root = power(b, x / 2)
      root * root
    }
  }

}


// trait FFT extends ScalaOpsPkg with LiftNumeric with Trig {
// trait FFT extends LiftNumeric with Trig {
// trait FFT extends LiftNumeric with Trig with Arith {
trait FFT extends LiftNumeric { this: Trig with Arith =>

  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))
  }

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)
    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)
    def *(that: Complex) = Complex(
      this.re * that.re - this.im * that.im,
      this.re * that.im + this.im * that.re)
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = {
    (xs: @unchecked) match {
      case e :: o :: xt =>
        val (es, os) = splitEvenOdd(xt)
        ((e :: es), (o :: os))
      case Nil => (Nil, Nil)
        // cases?
    }
  }

  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = {
    ((even, odd): @unchecked) match {
      case (Nil, Nil) =>
        Nil
      case ((e :: es), (o :: os)) =>
        e :: (o :: mergeEvenOdd(es, os))
        // cases?
    }
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

}

trait TrigExpOptFFT extends TrigExpOpt {
  override def cos(x: Exp[Double]) = x match {
    case Const(x) if {
      val z = x / math.Pi / 0.5; z != 0 && z == z.toInt
    } => Const(0.0)
    case _ => super.cos(x)
  }
}

trait Trig extends Base {

  def sin(x: Rep[Double]): Rep[Double]
  def cos(x: Rep[Double]): Rep[Double]

}

trait TrigExp extends Trig with BaseExp {

  case class Sin(x: Exp[Double]) extends Def[Double]
  case class Cos(x: Exp[Double]) extends Def[Double]

  def sin(x: Exp[Double]) = Sin(x)
  def cos(x: Exp[Double]) = Cos(x)
}

trait TrigExpOpt extends TrigExp {

  override def sin(x: Exp[Double]) = x match {
    case Const(x) => unit(math.sin(x))
    case _ => super.sin(x)
  }

  override def cos(x: Exp[Double]) = x match {
    case Const(x) => unit(math.cos(x))
    case _ => super.cos(x)
  }

}

trait ArithExpOptFFT extends ArithExpOpt {

  override def infix_+(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (x, Def(Plus(Const(0.0) | Const(-0.0), y))) => infix_+(x, y)
    case _ => super.infix_+(x, y)
  }

  override def infix_-(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (x, Def(Minus(Const(0.0) | Const(-0.0), y))) => infix_+(x, y)
    case _ => super.infix_-(x, y)
  }

  override def infix_*(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (x, Const(-1.0)) => infix_-(Const(0.0), x)
    case (Const(-1.0), y) => infix_-(Const(0.0), y)
    case _ => super.infix_*(x, y)
  }

}

trait ArithExpOpt extends ArithExp {

  override def infix_+(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case (Const(0.0) | Const(-0.0), y) => y
    case _ => super.infix_+(x, y)
  }

  override def infix_-(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case (x, Const(0.0) | Const(-0.0)) => x
    case _ => super.infix_-(x, y)
  }

  override def infix_*(x: Exp[Double], y: Exp[Double]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(0.0) | Const(-0.0)) => Const(0.0)
    case (Const(0.0) | Const(-0.0), y) => Const(0.0)
    case _ => super.infix_*(x, y)
  }
}

// TODO: not sure I need the rest; can probably make use of ScalaOpsPkg somehow

trait Arith extends Base {
  class arithOps(x: Rep[Double]){
    def +(y: Rep[Double]) = infix_+(x,y)
    def -(y: Rep[Double]) = infix_-(x,y)
    def *(y: Rep[Double]) = infix_*(x,y)
  }

  def infix_+(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def infix_-(x: Rep[Double], y: Rep[Double]): Rep[Double]
  def infix_*(x: Rep[Double], y: Rep[Double]): Rep[Double]
}

trait ArithExp extends Arith with BaseExp {
  case class Plus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Minus(x: Exp[Double], y: Exp[Double]) extends Def[Double]
  case class Times(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  def infix_+(x: Exp[Double], y: Exp[Double]) = Plus(x, y)
  def infix_-(x: Exp[Double], y: Exp[Double]) = Minus(x, y)
  def infix_*(x: Exp[Double], y: Exp[Double]) = Times(x, y)
}
