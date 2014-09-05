package cs590.week1

import org.scalatest.FunSuite

class InterpreterTest extends FunSuite with Interpreter1 {

  ignore("fact") {
    /**
      * TODO: interpret factorial function
      */
  }

}


class Interpreter2Test extends FunSuite with Interpreter2 {
  test("apply pred") {
    assert(interpret(App(Var("pred"), Const(3))) == 2)
    assert(interpret(App(Var("pred"), Const(42))) == 41)
  }

  test("apply eq1") {
    assert(interpret(App(App(Var("eq1"), Const(3)), Const(3))) === true)
    assert(interpret(App(App(Var("eq1"), Const(1)), Const(2))) === false)
  }

  test("cond pred eq 0") {
    def eq2(lhs: Exp, rhs: Exp) = App(App(Var("eq1"), lhs), rhs)
    def c(x: Val) = Const(x)
    assert(interpret(Cond(eq2(App(Var("pred"), c(1)), c(0)), c(2), c(3))) === 2)
    assert(interpret(Cond(eq2(App(Var("pred"), c(1)), c(7)), c(2), c(3))) === 3)
  }

  test("apply lambda (first order)") {
    def pred(x: Exp) = App(Var("pred"), x)
    def predpred(x: Exp) = App(Lam("n", pred(pred(Var("n")))), x)
    assert(interpret(predpred(Const(3))) === 1)
    assert(interpret(predpred(Const(-8))) === -10)
  }

  test("apply lambda (higher order)") {
    def isEqual(x: Val) = Lam("y", App(App(Var("eq1"), Var("y")), Const(x)))
    def callLambdaWith42(lam: Lam) =
      interpret(App(Lam("lam", App(Var("lam"), Const(42))), lam))
    assert(callLambdaWith42(isEqual(42)) === true)
    assert(callLambdaWith42(isEqual(13)) === false)
  }

  test("multiplication") {
    def mul2(x: Val, y: Val) = App(App(Var("mul1"), Const(x)), Const(y))
    assert(interpret(mul2(3, 4)) == 12)
    assert(interpret(mul2(-42, 1)) == -42)
  }

  test("fact") {
    def callFact(x: Val) =
      interpret(Letrec("fact",
        Lam("n", Cond(App(App(Var("eq1"), Const(0)), Var("n")),
          Const(1),
          App(App(Var("mul1"), Var("n")),
            App(Var("fact"), App(Var("pred"), Var("n")))))),
        App(Var("fact"), Const(x))))

    assert(callFact(5) === 5 * 4 * 3 * 2 * 1)
    assert(callFact(1) === 1)
    assert(callFact(0) === 1)
  }
}
