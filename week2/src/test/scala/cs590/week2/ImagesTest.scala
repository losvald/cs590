package cs590.week2

import org.scalatest.FunSuite

class ImagesTest extends FunSuite with Images with Codegen {

  test("allred") {

    def allRed(p: Point): Color = p match {
      case (x, y) => (x, Times(Const(0.4), y), Const(0), Const(1))
    }

    // NOTE: we can write this shorter as
    //
    //     def allRed(p: Point): Color =
    //       p match { case (x,y) => (x, 0.5 * y, 0, 1) }
    //
    // thanks to operator overloading (def *(that: DoubleE) = ...)
    // and implicit conversion
    // from Double to DoubleE (implicit def unit(x: Double) = ...).

    generateImage("allred.html", allRed)
  }

  test("checkers") {

    def checkers(p: Point): Color = p match {
      case (x, y) => (
        // using Const and Over is intentional to prevent evaluation by Scala
        // and let the opt() method does Constant Folding
        If(x % Over(1, 4) < Over(1, 4) / 2,
          If(y % Over(1, 4) < Over(1, 4) / 2, 0, 1),
          If(y % Over(1, 4) < Over(1, 4) / 2, 1, 0)),
        // create a different AST to test robustness of CSE
        If(x % (Const(1) / Const(4)) < Over(Const(1) / Const(4), 2),
          If(y % (Const(1) / 4) < Over(1, 2) / 4, 0, 1),
          If(y % Over(1.0, 4) < Const(1) / (Const(4) * 2), 1, 0)),
        // finally, do constant-folding using the help of Scala
        If(x % (1.0 / 4) < Over(1, 4) / 2,
          If(y % Over(1, 4) < 1.0 / 4 / 2, 0, 1),
          If(y % (1.0 / 4) < 1.0 / 4 / 2, 1, 0)),
        1)
    }

    generateImage("checkers.html", checkers)
  }

}
