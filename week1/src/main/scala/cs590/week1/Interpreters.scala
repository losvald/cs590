package cs590.week1

/**
  * Problem 2: Definitional Interpreters
  */


/**
  * Interpreter I from Reynolds' paper
  */

trait Interpreter1 {

  type Val = Any // Int, Boolean, Val => Val
  type Fun = Val => Val

  type Ident = String

  type Env = Ident => Val

  abstract class Exp
  case class Const(x: Val) extends Exp
  case class Var(s: Ident) extends Exp
  case class App(opr: Exp, opnd: Exp) extends Exp
  case class Lam(fp: Ident, body: Exp) extends Exp
  case class Cond(prem: Exp, conc: Exp, altr: Exp) extends Exp
  case class Letrec(dvar: Var, dexp: Lam, body: Exp) extends Exp
  case class Eq(lhs: Exp, rhs: Exp) extends Exp
  case class Minus(lhs: Exp, rhs: Exp) extends Exp
  case class Mult(lhs: Exp, rhs: Exp) extends Exp

  def eval(e: Exp, env: Env): Val = e match {
    case Const(x) => x
    case Var(x)   => env(x)
    case App(f,x) => eval(f,env).asInstanceOf[Fun](eval(x,env))
    case Lam(x,e) => evlambda(x,e,env)
    case Cond(p,c,a) => {
      if (eval(p,env).asInstanceOf[Boolean]) eval(c,env)
      else eval(a,env)
    }
    case Letrec(v,x,b) => {
      def env2(s: Ident): Val =
        if (s == v.s) evlambda(x.fp, x.body, env2) else env(s)
      eval(b, env2)
    }
    case Eq(lhs: Exp, rhs: Exp) => (lhs, rhs) match {
      // we need to be careful not to evaluate lambdas
      case (Lam(fp1, _), Lam(fp2, _)) => fp1 == fp2
      case (_, _) => eval(lhs, env) == eval(rhs, env)
    }
    case Minus(lhs: Exp, rhs: Exp) => (eval(lhs, env), eval(rhs, env)) match {
      case (lhs: Int, rhs: Int) => lhs - rhs
      case (lhs: Boolean, rhs: Boolean) => lhs && !rhs
    }
    case Mult(lhs: Exp, rhs: Exp) => (eval(lhs, env), eval(rhs, env)) match {
      case (lhs: Int, rhs: Int) => lhs * rhs
      case (lhs: Boolean, rhs: Boolean) => lhs && rhs
    }
  }

  def evlambda(x: Ident, e: Exp, env: Env) = {
    (a: Val) => {
      val env2 = ext(x,a,env)
      eval(e, env2)
    }
  }
  def ext(z: Ident, a: Val, env: Env): Env = {
    (s: Ident) => if (s == z) a else env(s)
  }
}


/**
  * Interpreter II from Reynolds' paper
  */

trait Interpreter2 {

  type Ident = String

  type Val = Any // Int, Boolean, Val => Val

  abstract class Fun
  case class Clos(lam: Lam, env: Env) extends Fun
  case class Pred() extends Fun
  case class EQ1() extends Fun
  case class EQ2(arg1: Val) extends Fun
  case class Mul1() extends Fun
  case class Mul2(arg1: Val) extends Fun

  abstract class Env
  case class Init() extends Env
  case class Simp(bvar: Ident, bval: Val, old: Env) extends Env
  case class Rec(letx: Letrec, old: Env) extends Env

  abstract class Exp
  case class Const(x: Val) extends Exp
  case class Var(s: Ident) extends Exp
  case class App(opr: Exp, opnd: Exp) extends Exp
  case class Lam(fp: Ident, body: Exp) extends Exp
  case class Cond(prem: Exp, conc: Exp, altr: Exp) extends Exp
  case class Letrec(dvar: Ident, dexp: Lam, body: Exp) extends Exp

  def interpret(r: Exp) = eval(r, Init())

  def eval(r: Exp, e: Env): Val = r match {
    case Const(x) => x
    case Var(s) => get(e, s)
    case App(opr, opnd) => apply(eval(opr, e).asInstanceOf[Fun], eval(opnd, e))
    case r: Lam => Clos(r, e)
    case Cond(prem, conc, altr) => {
      if (eval(prem, e).asInstanceOf[Boolean]) eval(conc, e) else eval(altr, e)
    }
    case r: Letrec => eval(r.body, Rec(r, e))
  }

  def apply(f: Fun, a: Val): Val = f match {
    // TODO: just like in Interpreter1, we could enforce strong typing
    // (e.g. case (a: Boolean, b: Boolean) => ... case (a: Int, b: Int) => ...)
    case Clos(Lam(fp, body), env) => eval(body, Simp(fp, a, env))
    case Pred() => a.asInstanceOf[Int] - 1
    case EQ1() => EQ2(a)
    case EQ2(b) => a == b
    case Mul1() => Mul2(a)
    case Mul2(b) => a.asInstanceOf[Int] * b.asInstanceOf[Int]
  }

  def get(e: Env, x: Ident): Val = e match {
    case Init() => x match {
      case "pred" => Pred()
      case "eq1" => EQ1()
      case "mul1" => Mul1()
    }
    case Simp(bvar, bval, eOld) => if (x == bvar) bval else get(eOld, x)
    case Rec(letx, eOld) => {
      if (x == letx.dvar) Clos(letx.dexp, e) else get(eOld, x)
    }
  }
}
