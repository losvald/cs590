package cs590.week1

import org.scalatest.FunSuite

class InterpreterTest extends FunSuite with Interpreter1 {
  test("Mult Int") {
    assert(eval(Mult(Const(3), Const(4)), _ => 0) === 12)
    assert(eval(Mult(Const(3), Const(0)), _ => 0) === 0)
  }

  test("Mult Boolean") {
    val env0 = (x: Ident) => false
    assert(eval(Mult(Const(false), Const(true)), env0) === false)
    assert(eval(Mult(Const(true), Const(false)), env0) === false)
    assert(eval(Mult(Const(false), Const(false)), env0) === false)
    assert(eval(Mult(Const(true), Const(true)), env0) === true)
  }

  test("Minus") {
    assert(eval(Minus(Const(5), Const(3)), _ => 0) === 2)
    for (env0 <- Seq((x: Ident) => false, (x: Ident) => true)) {
      assert(eval(Minus(Const(true), Const(false)), env0) === true)
      assert(eval(Minus(Const(false), Const(false)), env0) === false)
    }
  }

  test("Cond Eq Int") {
    val c1 = Const(1)
    assert(eval(Cond(Eq(c1, c1), Const(2), Const(3)), _ => 0) === 2)
    assert(eval(Cond(Eq(c1, Const(4)), Const(2), Const(3)), _ => 0) === 3)
  }

  test("Cond Eq Lam") {
    val tauto = Lam("tauto", Const(false))
    val fakeTauto = Lam("tauto2", Const(false))
    val implTauto = Lam("tauto", Const(0))
    assert(eval(Cond(Eq(tauto, tauto), Const(1), Const(2)), _ => 0) === 1)
    assert(eval(Cond(Eq(tauto, fakeTauto), Const(1), Const(2)), _ => 0) === 2)
    assert(eval(Cond(Eq(tauto, implTauto), Const(1), Const(2)), _ => 0) === 1)
  }

  test("apply lambda (first order)") {
    def evalMinus2(n: Val) =
      eval(App(Lam("n", Minus(Var("n"), Const(2))), Const(n)), _ => 0)
    assert(evalMinus2(5) === 3)
    assert(evalMinus2(1) === -1)
  }

  test("apply lambda (higher order)") {
    def minusN(n: Val) = Lam("x", Minus(Var("x"), Const(n)))
    def callLambdaWith42(lam: Lam) =
      eval(App(Lam("lam", App(Var("lam"), Const(42))), lam), _ => 0)
    assert(callLambdaWith42(minusN(84)) === -42)
    assert(callLambdaWith42(minusN(0)) === 42)
  }

  test("fact") {
    def callFact(n: Val) = eval(
      Letrec(
        Var("fact"), Lam("n",
          Cond(Eq(Var("n"), Const(0)),
            Const(1),                     // terminating case (0! == 1)
            Mult(Var("n"), App(Var("fact"), Minus(Var("n"), Const(1)))))),
        App(Var("fact"), Const(n))),
      _ => 0)
    assert(callFact(5) === 1 * 2 * 3 * 4 * 5)
    assert(callFact(1) === 1)
    assert(callFact(0) === 1)
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
