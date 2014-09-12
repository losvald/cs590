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
  }
  case class Const(d: Double) extends DoubleE
  case class Sym(x: String) extends DoubleE
  case class Times(a: DoubleE, b: DoubleE) extends DoubleE
  case class Over(a: DoubleE, b: DoubleE) extends DoubleE
  case class Modulo(a: DoubleE, b: DoubleE) extends DoubleE
  case class LT(a: DoubleE, b: DoubleE) extends DoubleE
  case class If(prem: DoubleE, conc: DoubleE, altr: DoubleE) extends DoubleE

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
    case _ => e
  }

  def eval(e: DoubleE): String = opt(e) match {
    case Sym(x) => x
    case Const(d) => d.toString
    case Times(a, b) => s"(${eval(a)} * ${eval(b)})"
    case Over(a, b) => s"(${eval(a)} / ${eval(b)})"
    case Modulo(a, b) => s"(${eval(a)} % ${eval(b)})"
    case LT(a, b) => s"(${eval(a)} < ${eval(b)})"
    case If(prem, conc, altr) => {
      s"(${eval(prem)} ? ${eval(conc)} : ${eval(altr)})"
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
