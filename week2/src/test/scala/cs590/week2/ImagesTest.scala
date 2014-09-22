package cs590.week2

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class ImagesTest extends FunSuite with Images with Codegen with BeforeAndAfter {

  before {
    printOrd = -1 //3
  }

  after {
    System.err.println()
  }

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

  def checkers(p: Point): Color =
    p match {
      // Note: the following might not work if x or y is negative
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

  test("checkers") {
    generateImage("checkers.html", checkers)
  }

  test("implicit conversion") {
    val p0: Point = (2, 3)
    val pe: PointE = p0
    val p: Point = pe
    assert(p === p0)
  }

  test("checkers transformed") {
    def checkers_transformed(p: Point) =
      // TODO: find a cleaner way to construct the point (3.1, 2.4)
      checkers(Rot(p * 2 + ((3.1, 2.4): Point),
        If(p.x + p.y < 1.5,
          (p.x - 0.2) * (p.x - 0.2) + (p.y - 0.5) * (p.y - 0.5),
          0.5)))

    generateImage("checkers-transformed.html", checkers_transformed)
  }

  test("no CSE - each symbol once") {
    val e = Plus(
      Times(
        Times(Sym("a"), Sym("b")),
        Times(Sym("c"), Sym("d"))),
      Plus(Sym("e"), Sym("f")))
    assert(eval(e).toString === "(((a * b) * (c * d)) + (e + f))")
  }

  test("CSE - either side (x * 2)") {
    // printOrd = 1
    val cseSym = Sym("_cse1")
    assert(eval(
      Plus(
        Times(Sym("x"), 2),
        Times(Sym("x"), 2))
    ) === eval(Let(cseSym, Times(Sym("x"), 2),
      Plus(
        cseSym,
        cseSym))))
  }

  test("CSE - lowest common ancestor") {
    // printOrd = 1
    val cseSym = Sym("_cse1")
    val cse = Times(Sym("x"), 2)
    assert(eval(
      Compl(
        Plus(
          Sym("y"),
          Over(
            cse,
            Cos(
              Modulo(cse, Sym("z"))))))
    ) === eval(
      Compl(
        Plus(
          Sym("y"),
          Let(cseSym, cse,
            Over(
              cseSym,
              Cos(
                Modulo(cseSym, Sym("z")))))))))
  }

  test("CSE - same conc & altr") {
    // printOrd = 1
    val cseSym = Sym("_cse1")
    assert(eval(
      If(
        Sym("c"),
        Times(Sym("a"), Sym("b")),
        Times(Sym("a"), Sym("b")))
    ) === eval(Let(cseSym, Times(Sym("a"), Sym("b")),
      If(Sym("c"),
        cseSym,
        cseSym))))
  }

  test("no CSE - either conc or altr") {
    // printOrd = 1
    val cseSym = Sym("_cse1")
    assert(eval(
      Plus(
        Times(Sym("a"), Sym("b")),
        If(
          Sym("c"),
          Times(Sym("a"), Sym("b")),
          Const(42)))
    ) !== eval(Let(cseSym, Times(Sym("a"), Sym("b")),
      Plus(
        cseSym,
        If(Sym("c"),
          cseSym,
          Const(42))))))
  }

  test("no CSE - prem and either conc or altr") {
    // printOrd = 1
    val cseSym = Sym("_cse1")
    assert(eval(
      If(
        Times(Sym("a"), Sym("b")),
        Times(Sym("a"), Sym("b")),
        Const(42))
    ) !== eval(Let(cseSym, Times(Sym("a"), Sym("b")),
      If(cseSym, cseSym, Const(42)))))
  }

  test("no CSE - safe prem and unsafe either") {
    // printOrd = 1
    val cse = Plus(Sym("a"), Sym("b"))
    val cseSym = Sym("_cse1")
    assert(eval(
      If(cse,
        cse,
        If(Sym("x"),
          Const(1),
          cse))
    ) === eval(
      If(cse, // not subst'd because of unsafe branch (see below)
        cse, // not subst'd because of unsafe branch (see below)
        If(Sym("x"),
          Const(1),
          cse)))) // not subst'd because unsafe (depends on the innermost If)
  }

  test("CSE - subst safe only in prem (unsafe nested in safe's parent)") {
    // printOrd = 1
    val cse = Modulo(Sym("a"), Sym("b"))
    val cseSym = Sym("_cse1")
    assert(eval(
      If(
        Plus(
          cse,
          Times(
            cse,
            If(Sym("x"),
              cse,
              Const(0)))),
        Const(1),
        Const(2))
    ) === eval(
      If(Let(cseSym, cse,
        Plus( // nesting inside promise doesn't make it unsafe
          cseSym,
          Times(
            cseSym,
            If(Sym("x"),
              cse, // not subst'd because unsafe
              Const(0))))),
        Const(1),
        Const(2))))
  }

  test("nested CSE - prem nested in both branches") {
    // printOrd = 1
    val cseCos = Sym("_cse1")
    val cseSquare = Sym("_cse2")
    assert(eval(
      If(
        Compl(
          Times(Sym("x"), Sym("x"))), // cseSquare
        Cos(
          Times(Sym("x"), Sym("x"))), // cseSquare
        Sin(
          Cos( // cseCos
            Times(Sym("x"), Sym("x"))))) // cseSquare
    ) === eval(
      Let(cseSquare, Times(Sym("x"), Sym("x")),
        Let(cseCos, Cos(cseSquare),
          If(
            Compl(cseSquare),
            cseCos,
            Sin(cseCos))))))
  }

  // Note: these two don't pass because I haven't implemented the
  // optimization to pull up and merge similar lets
  // it's tricky because of the following scenario:
  //  Let(e1, E1,
  //     Let(e2, E2,  // if E2 contains e1, how can we pull it???
  //       ...
  // (I doubt that checking whether we can pull E1 recursively suffices,
  // since we might run into similar problems and worry about safeness...)
  // I guess that is the reason why Conal et al.'s paper separates the
  // process of letifying into incremental process of:
  /// 1) letify
  //  2) optimize
  // TODO: Is it correct?

  ignore("CSE - pull up and merge with right let") {
    printOrd = 1
    val cseSym = Sym("_cse1")
    val cse = Times(Sym("a"), Sym("b"))
    assert(eval(
      Times(Plus(cse, cse), cse)
    ) === eval(Let(cseSym, cse,
      Times(Plus(cseSym, cseSym), cseSym))))
  }

  ignore("CSE - in all outcomes") {
    printOrd = 1
    val cse = Plus(Sym("c1"), Sym("c2"))
    val cseSym = Sym("_cse1")
    assert(eval(
      If(Sym("a0"),
        If(Sym("a1"),
          Times(
            Sym("b0"),
            If(Sym("c0"),
              cse,
              Compl(Times(cse, Sym("d1"))))),
          cse),
        If(Sym("a2"),
          cse,
          Sin(cse)))
    ) === eval(Let(cseSym, cse,
      If(Sym("a0"),
        If(Sym("a1"),
          Times(
            Sym("b0"),
            If(Sym("c0"),
              cseSym,
              Compl(Times(cseSym, Sym("d1"))))),
          cseSym),
        If(Sym("a2"),
          cseSym,
          Sin(cseSym))))))
  }
}
