package cs590.week2

import scala.language.implicitConversions
import scala.collection.immutable.Queue
import scala.collection.immutable.Stack
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
import util.control.Breaks._

trait Images {

  type Image = Point => Color
  type Point = (DoubleE, DoubleE)
  type Color = (DoubleE, DoubleE, DoubleE, DoubleE)

  abstract class DoubleE {
    def *(that: DoubleE) = Times(this, that)
    def /(that: DoubleE) = Over(this, that)
    def %(that: DoubleE) = Modulo(this, that)
    def <(that: DoubleE) = LT(this, that)
    def +(that: DoubleE) = Plus(this, that)
    // express - in terms of + and compl. to reduce number of cases in opt()
    def -(that: DoubleE) = Plus(this, Compl(that))
    // def unary_-: DoubleE = Compl(this)
  }
  abstract class DoubleEBin(a: DoubleE, b: DoubleE) extends DoubleE {
    // def unapply(binOp: DoubleEBin) = Some((binOp.a, binOp.b))
  }
  case class Const(d: Double) extends DoubleE
  case class Sym(x: String) extends DoubleE
  case class Times(a: DoubleE, b: DoubleE) extends DoubleEBin(a, b)
  case class Over(a: DoubleE, b: DoubleE) extends DoubleEBin(a, b)
  case class Modulo(a: DoubleE, b: DoubleE) extends DoubleEBin(a, b)
  case class LT(a: DoubleE, b: DoubleE) extends DoubleEBin(a, b)
  case class If(prem: DoubleE, conc: DoubleE, altr: DoubleE) extends DoubleE
  case class Plus(a: DoubleE, b: DoubleE) extends DoubleEBin(a, b)
  case class Sin(a: DoubleE) extends DoubleE
  case class Cos(a: DoubleE) extends DoubleE
  case class Compl(a: DoubleE) extends DoubleE
  case class Let(sym: Sym, exp: DoubleE, body: DoubleE) extends DoubleE

  object Binary {
    def unapply(e: Times) = Times.unapply(e)
    def unapply(e: Over) = Over.unapply(e)
  }

  // let point expressions behave like macros that push evaluation inside,
  // rather than having to do extra work in eval()
  abstract class PointE(val x: DoubleE, val y: DoubleE) {
    def +(d: PointE) = Trans(this, d)
    def *(f: DoubleE) = Scale(this, f)
    def toPoint = (x, y)
  }
  case class ID(p: Point) extends PointE(p._1, p._2)
  case class Trans(p: PointE, d: PointE) extends PointE(p.x + d.x, p.y + d.y)
  case class Scale(p: PointE, f: DoubleE) extends PointE(f * p.x, f * p.y)
  case class Rot(p: PointE, a: DoubleE) extends PointE(
    p.x * Cos(a) - p.y * Sin(a),
    p.x * Sin(a) + p.y * Cos(a))

  implicit def pointE2Point(e: PointE) = e.toPoint
  implicit def point2PointE(p: Point) = ID(p)
  implicit def unit(d: Double): DoubleE = Const(d)

}

trait ImagesPoly {

  // see section 5 in Elliott's paper

  type Image = Point => Color
  type Point = (DoubleE, DoubleE)
  type Color = (DoubleE, DoubleE, DoubleE)

  type DoubleE = Exp[Double]

  abstract class Exp[T]

  case class Const[T](d: T) extends Exp[T]
  case class Sym[T](x: String) extends Exp[T]
  case class Times(a: Exp[Double], b: Exp[Double]) extends Exp[Double]

  implicit def unit[T](d: T) = Const(d)

  implicit class DoubleOps(a: DoubleE) {
    def *(b: DoubleE) = Times(a, b)
  }

}

trait Codegen extends Images {

  def writeFile(name: String, content: String) {
    val out = new java.io.PrintWriter(new java.io.File(name))
    out.write(content)
    out.close()
  }

  def generateImage(fileName: String, image: Image) =
    writeFile(fileName, template(fileName, image))

  def opt(e: DoubleE): DoubleE = e match {
    case Times(a, b) => Times(opt(a), opt(b)) match {
      case Times(a: Const, b: Const) => Const(a.d * b.d)
      case default => default
    }
    // analogous to the Times folding, but less verbose
    case Over(a, b) => (opt(a), opt(b)) match {
      case (a: Const, b: Const) => Const(a.d / b.d)
      case (a, b) => Over(a, b)
    }
    // similarly, we could optimize the rest, but we just decide to propagate
    case Modulo(a, b) => (opt(a), opt(b)) match {
      case (a: Const, b: Const) => Const(a.d % b.d)
      case (a, b) => Modulo(a, b)
    }
    case LT(a, b) => (opt(a), opt(b)) match {
      case (a: Const, b: Const) => Const(if (a.d < b.d) 1.0 else 0.0)
      case (a, b) => LT(a, b)
    }
    case If(prem, conc, altr) => opt(prem) match {
      case Const(prem) => if (Math.abs(prem) > 1e-9) opt(conc) else opt(altr)
      case prem => If(prem, opt(conc), opt(altr))
    }
    case Plus(a, b) => (opt(a), opt(b)) match {
      case (a: Const, b: Const) => Const(a.d + b.d)
      case (a, b) => Plus(a, b)
    }
    case Sin(a) => Sin(opt(a))
    case Cos(a) => Cos(opt(a))
    case c: Compl => opt(c.a) match {
      case Compl(a) => a
      case _ => c
    }
    case _ => e
  }

  var printOrd = -1 // for debugging, set by unittest in before/after()

  def cse(eTopLevel: DoubleE): DoubleE = {
    val ret = eTopLevel
    var letId = 0

    def letify(e: DoubleE, s1: HashSet[DoubleE], s2: HashSet[DoubleE]):
        Tuple2[DoubleE, HashSet[DoubleE]] = {
      if (printOrd == 0)
        System.err.print("Letifying: " + e + "[\n" + s1 + "\n|\n" + s2 + "\n]")
      val sCommon = s1 & s2
      if (sCommon.isEmpty) {
        if (printOrd == 0) System.err.println()
        return (e, s1 ++ s2 + e)
      }

      if (printOrd == 0) System.err.println(" =(s1&s2)=> " + sCommon)

      // We need to topologically sort because upon merging children sets
      // several nesting CSE can appear.  For example,
      //    if               Let(e1, E1,
      //   /|\                 Let(e2, E2,
      //  / | \                     if
      // E1 E2 E4      --->        /|\
      //   / |  \                 / | \
      //  E3 E1 E2               e1 e2 E4
      //        | \                     \
      //       E3 E1                    e2
      // must yield Let(e1, E1, Let(e2, E2, ...)), not Let(e2, E2, Let(e1, ...))
      // (note that nesting cannot contradict because of deep equality,
      // e.g. E2 cannot be a descendant of E1, nor can E1 be an ancestor of E4).
      // Therefore, e2 must be pulled up (substituted for E2) before e1
      // (this also prunes unnecessary substitions of E1 within E2's subtree).
      def topoSort(q0: Queue[DoubleE]): Stack[DoubleE] = {
        // val e = q0.dequeue match {
        // def caseBinOp[A <: DoubleEBin](e: A): DoubleE = {
        //   e.a
        // }
        try {
          val (e, q) = q0.dequeue
          val sorted = e match {
            // TODO: can I somehow match against type [T <: DoubleE] T(a, b)?
            // case DoubleEBin(a, b) => topoSort(q :+ e.a :+ e.b)
            // case Binary(a, b) => topoSort(q :+ a :+ b)
            case Times(a, b) => topoSort(q :+ a :+ b)
            case Over(a, b) => topoSort(q :+ a :+ b)
            case Modulo(a, b) => topoSort(q :+ a :+ b)
            case LT(a, b) => topoSort(q :+ a :+ b)
            case Plus(a, b) => topoSort(q :+ a :+ b)
            case If(prem, conc, altr) => topoSort(q :+ prem :+ conc :+ altr)
            case Sin(a) => topoSort(q :+ a)
            case Cos(a) => topoSort(q :+ a)
            case Compl(a) => topoSort(q :+ a)
            case _ => topoSort(q)
          }
          if (sCommon.contains(e)) sorted.push(e) else sorted
        } catch {
          case _: NoSuchElementException => Stack[DoubleE]()
        }
      }
      val sCommonSorted = topoSort(Queue(e))
      if (printOrd == 0) System.err.println(" =(topoSort)=> " + sCommonSorted)

      var eWithLets = e // XXX: make this pure using foldLeft instead of for
      var sCommonBound = HashSet[DoubleE]()
      for (common <- sCommonSorted) breakable {
        if (sCommonBound.contains(common)) {
          if (printOrd == 0)
            System.err.println(" SKIPPED repeated CSE: " + common)
          break // proceed with the next iteration of the for-loop
        }

        sCommonBound = sCommonBound + common
        letId += 1
        val pullSym = Sym("_cse" + letId)

        def pullUp(e: DoubleE, topLevel: Boolean = false): DoubleE = {
          if (e == common) pullSym
          else e match {
            case Let(sym, exp, body) => {
              // Note that common might be bound in exp of a Let, not only body,
              // and that let should be skipped when determining topLevel.
              Let(sym, pullUp(exp), pullUp(body, topLevel))
            }
            case If(prem, conc, altr) => {
              // XXX: no special care required for If because, by transitiviy,
              // XXX: only exprs common to both branches are examined
              // Do not pull up from either branch since it is not safe
              // (if they share a CSE, it would've been pulled up already),
              // unless it is the topLevel (the first non-Let expr to execute)
              if (topLevel) If(pullUp(prem), pullUp(conc), pullUp(altr))
              else If(pullUp(prem), conc, altr)
            }
            case Times(a, b) => Times(pullUp(a), pullUp(b))
            case Modulo(a, b) => Modulo(pullUp(a), pullUp(b))
            case Over(a, b) => Over(pullUp(a), pullUp(b))
            case LT(a, b) => LT(pullUp(a), pullUp(b))
            case Plus(a, b) => Plus(pullUp(a), pullUp(b))
            case Sin(a) => Sin(pullUp(a))
            case Cos(a) => Cos(pullUp(a))
            case Compl(a) => Compl(pullUp(a))
            case _ => e
          }
        }
        eWithLets = Let(pullSym, common, pullUp(eWithLets, true))
        if (printOrd == 0) {
          System.err.println(" PULLED UP: " + pullSym + " @ " + common)
          System.err.println("  =(pullUp)=>" + eWithLets)
        }
      }
      if (printOrd == 0)
        System.err.println(" =(letify)=> " + eWithLets)
      (eWithLets, s1 ++ s2 + e)
    }

    def cse1(e: DoubleE): Tuple2[DoubleE, HashSet[DoubleE]] = {
      val (e2, s2) = e match {
        case If(prem, conc, altr) => {
          val (conc2, concS) = cse1(conc)
          val (altr2, altrS) = cse1(altr)
          val commonS = concS & altrS
          val (prem2, premS) = cse1(prem)
          // First, we pull up CSEs that are shared by both branches.  Then,
          // we pull up CSEs that are shared among prem and either branch.
          val (either, eitherS) = letify(If(prem2, conc2, altr2), concS, altrS)
          letify(either, premS, concS & altrS)
        }
        // TODO: How to avoid these boilerplate repetition in match cases?
        case Times(a, b) => {
          val ((a2, aS), (b2, bS)) = (cse1(a), cse1(b))
          letify(Times(a2, b2), aS, bS)
        }
        case Modulo(a, b) => {
          val ((a2, aS), (b2, bS)) = (cse1(a), cse1(b))
          letify(Modulo(a2, b2), aS, bS)
        }
        case Over(a, b) => {
          val ((a2, aS), (b2, bS)) = (cse1(a), cse1(b))
          letify(Over(a2, b2), aS, bS)
        }
        case LT(a, b) => {
          val ((a2, aS), (b2, bS)) = (cse1(a), cse1(b))
          letify(LT(a2, b2), aS,  bS)
        }
        case Plus(a, b) => {
          val ((a2, aS), (b2, bS)) = (cse1(a), cse1(b))
          letify(Plus(a2, b2), aS, bS)
        }
        case Sin(a) => {
          val (a2, aS) = cse1(a)
          (Sin(a), aS + e)
        }
        case Cos(a) => {
          val (a2, aS) = cse1(a)
          (Cos(a2), aS + e)
        }
        case Compl(a) => {
          val (a2, aS) = cse1(a)
          (Compl(a2), aS + e)
        }
        // the remaining operations are too cheap for CSE (Const, Sym, etc.)
        case _ => (e, HashSet[DoubleE]())
      }
      (e2, s2)
    }
    val (e2, s2) = cse1(eTopLevel)
    if (printOrd == 0)
      System.out.println("=(cse1)=> " + e2)

    e2
  }

  def eval(e: DoubleE): String = {
    printOrd -= 1
    if (printOrd == 0) {
      System.err.println("> eval(\n" + e.toString)
    }
    val eOpt = opt(e)
    if (printOrd == 0) {
      if (e.toString != eOpt.toString) {
        System.err.println(") =(opt)=> eval(\n" + eOpt.toString)
      }
      System.err.println(")")
    }
    val ret = eval0(cse(eOpt))
    if (printOrd == 0)
      System.err.println("=(eval)=> " + ret)
    ret
  }

  def eval0(e: DoubleE): String = e match {
    case Sym(x) => x
    case Const(d) => d.toString
    case Times(a, b) => s"(${eval0(a)} * ${eval0(b)})"
    case Over(a, b) => s"(${eval0(a)} / ${eval0(b)})"
    case Modulo(a, b) => s"(${eval0(a)} % ${eval0(b)})"
    case LT(a, b) => s"(${eval0(a)} < ${eval0(b)})"
    case If(prem, conc, altr) => {
      s"(${eval0(prem)} ? ${eval0(conc)} : ${eval0(altr)})"
    }
    case Plus(a, b) => s"(${eval0(a)} + ${eval0(b)})"
    case Sin(a) => s"Math.sin(${eval0(a)})"
    case Cos(a) => s"Math.cos(${eval0(a)})"
    case Compl(a) => s"(-${eval0(a)})"
    case Let(sym, exp, body) => {
      // use JS closure & application to achieve the functional effect of Let
      s"function(${eval0(sym)}){ return ${eval0(body)}; }(${eval0(exp)})"
    }
  }

  def template(fileName: String, image: Image) = s"""
    <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
    <html>
    <head>
      <title>CS590: $fileName</title>
      <style>
      canvas { cursor: crosshair; }
      </style>
      <script>
        function drawImage() {
          var w = 300;
          var h = 300;
          var gCanvas = document.getElementById("canvas");

          var gCtx = gCanvas.getContext("2d");
          gCtx.fillRect(0,0,w,h);

          var imageData = gCtx.getImageData(0,0,w,h);

          for (var x = 0; x < imageData.width; x++) {
            for (var y = 0; y < imageData.height; y++) {
              var offset = (y * imageData.width + x) * 4;

              var xs = x/w;
              var ys = y/h;

              var r = 0;
              var g = 0;
              var b = 0;
              var a = 0;

              ${
                val (r,g,b,a) = image((Sym("xs"),Sym("ys")))
                val (rs,gs,bs,as) = (eval(r), eval(g), eval(b), eval(a))
                s"r = $rs*255; g = $gs*255; b = $bs*255; a = $as*255"
              }

              imageData.data[offset] = r;
              imageData.data[offset + 1] = g;
              imageData.data[offset + 2] = b;
              imageData.data[offset + 3] = a;
            }
          }
          gCtx.putImageData(imageData, 0, 0);
        }
      </script>
    </head>
    <body onload="drawImage()">
      <h1>Image: $fileName</h1>
      <div id="container" align="center">
        <canvas id="canvas" width="300" height="300" style="border: 1px solid black;"/>
      </div>
    </body>
    </html>"""

}
