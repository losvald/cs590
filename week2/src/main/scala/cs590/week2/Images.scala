package cs590.week2

import scala.language.implicitConversions

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
  case class Const(d: Double) extends DoubleE
  case class Sym(x: String) extends DoubleE
  case class Times(a: DoubleE, b: DoubleE) extends DoubleE
  case class Over(a: DoubleE, b: DoubleE) extends DoubleE
  case class Modulo(a: DoubleE, b: DoubleE) extends DoubleE
  case class LT(a: DoubleE, b: DoubleE) extends DoubleE
  case class If(prem: DoubleE, conc: DoubleE, altr: DoubleE) extends DoubleE
  case class Plus(a: DoubleE, b: DoubleE) extends DoubleE
  case class Sin(a: DoubleE) extends DoubleE
  case class Cos(a: DoubleE) extends DoubleE
  case class Compl(a: DoubleE) extends DoubleE

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
    case Modulo(a, b) => Modulo(opt(a), opt(b))
    case LT(a, b) => LT(opt(a), opt(b))
    case If(prem, conc, altr) => opt(prem) match {
      case Const(prem) => if (Math.abs(prem) > 1e-9) opt(conc) else opt(altr)
      case prem => If(prem, opt(conc), opt(altr))
    }
    case Plus(a, b) => Plus(opt(a), opt(b))
    case Sin(a) => Sin(opt(a))
    case Cos(a) => Cos(opt(a))
    case c: Compl => opt(c.a) match {
      case Compl(a) => a
      case _ => c
    }
    case _ => e
  }

  def eval(e: DoubleE): String = eval0(opt(e))

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
