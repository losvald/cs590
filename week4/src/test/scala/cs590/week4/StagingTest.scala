package cs590.week4

import scala.virtualization.lms.common._
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import java.io.{ByteArrayOutputStream, PrintWriter}

import scala.language.implicitConversions

// TODO: not sure I need theses, nor the ScalaGen* classes below
// But using ScalaOpsPkgExp creates a conflict with arrayApply()...

trait Arrays extends Base {
  class ArrayOps[T:Manifest](x: Rep[Array[T]]) {
    def apply(i: Int) = arrayApply(x, i)
  }
  implicit def array2arrayOps[T:Manifest](x: Rep[Array[T]]) = new ArrayOps(x)

  def arrayApply[T:Manifest](x: Rep[Array[T]], i:Int): Rep[T]
  def makeArray[T:Manifest](x: List[Rep[T]]): Rep[Array[T]]
}

trait ArraysExp extends Arrays with BaseExp {
  case class ArrayApply[T:Manifest](x:Rep[Array[T]], i:Int) extends Def[T]
  case class MakeArray[T:Manifest](x:List[Rep[T]]) extends Def[Array[T]]

  def arrayApply[T:Manifest](x: Rep[Array[T]], i:Int) = ArrayApply(x, i)
  def makeArray[T:Manifest](x: List[Rep[T]]) = MakeArray(x)
}


class StagingTest extends FunSuite with org.scalatest.BeforeAndAfter {

  val eps = 1e-9

  def filter_mults(code: String) =
    code split "\n" filter { _.contains(" * ") } mkString "\n"

  test("power1") {
    val driver = new Power1 with ScalaOpsPkgExp with CompileScala { self =>
      val codegen = new ScalaCodeGenPkg { val IR: self.type = self }
    }

    var src_mults = "" // multiplications in the generated code

    val power4c = {
      import driver._

      val power4 = (x:Rep[Double]) => power(x,4)

      val baos = new ByteArrayOutputStream
      codegen.emitSource(power4, "Power4", new PrintWriter(baos))
      src_mults = filter_mults(baos.toString)

      compile(power4)
    }

    // verify the generated code has the expected 4 multiplications
    assert(src_mults === """
val x1 = x0 * 1.0
val x2 = x0 * x1
val x3 = x0 * x2
val x4 = x0 * x3
""".trim())

    assert(power4c(3) === 81.0 +- eps)
    assert(power4c(42) === 3111696.0 +- eps)
  }

  test("power2") {
    val driver = new Power2 with ScalaOpsPkgExp with CompileScala { self =>
      val codegen = new ScalaCodeGenPkg { val IR: self.type = self }
    }

    var src_mults = "" // multiplications in the generated code

    val power845c = {
      import driver._

      val power845 = (x:Rep[Double]) => power(x,845)

      val baos = new ByteArrayOutputStream
      codegen.emitSource(power845, "Power845", new PrintWriter(baos))
      src_mults = filter_mults(baos.toString)

      compile(power845)
    }

    // verify the generated code has the expected O(log(n)) multiplications
    assert(src_mults === """
val x1 = x0 * 1.0
val x2 = x1 * x1
val x3 = x0 * x2
val x4 = x3 * x3
val x5 = x4 * x4
val x6 = x0 * x5
val x7 = x6 * x6
val x8 = x7 * x7
val x9 = x8 * x8
val x10 = x0 * x9
val x11 = x10 * x10
val x12 = x0 * x11
val x13 = x12 * x12
val x14 = x13 * x13
val x15 = x0 * x14
""".trim())
    assert(power845c(1) === 1)
    assert(power845c(1.002) === 5.41 +- 0.001)
    assert(power845c(0.999) === 0.42 +- 0.01)

    // Note: MetaOCaml generates an inefficient code with lots of replications,
    // but the LMS approach does not, fortunately!
  }

  trait FFTStaged extends FFT with ArraysExp { this: TrigExp with ArithExp =>
    // with ArithExpOptFFT with TrigExpOptFFT =>
    def fftc(size: Int) = (input: Rep[Array[Double]]) => {
      // assume the size is a power of 2
      val arg = List.tabulate(size) {
        i => Complex(input(2 * i), input(2 * i + 1))
      }
      val res = fft(arg)
      makeArray(res.flatMap { case Complex(re, im) => List(re, im) })
    }
  }

  // TODO: How to pass variadic list of traits as args to mkFFTDriver
  // and have it return an instance of FFTStaged with those traits injected?
  // (e.g. mkFFTDriver[ArithExpOptFFT, TrigExp] should return
  //  new FFTStaged with ArithExpOpt with TrigExp with CompileScala)

  def mkFFTDriver() = new FFTStaged //with ScalaOpsPkgExp
      with CompileScala
  // TODO: Disable optimizations?  The following "GenerationFailedException:
  // don't know how to generate code for: Cos(Const(-0.0))":
  //  with TrigExp with ArithExp {  // these are required by the self-type
      with ArithExpOptFFT with TrigExpOptFFT {
    self => val codegen =
      new ScalaGenFlat with ScalaGenArrays with ScalaGenArith { // XXX: dirty
        val IR: self.type = self
      }
  }

  test("fft 2") {
    val driver = mkFFTDriver
    val fft2c = {
      import driver._
      val fft2 = fftc(2)
      codegen.emitSource(fft2, "FFT2", new PrintWriter(System.out))
      compile(fft2)
    }

    assert(fft2c(Array(1.0, 2.0, 3.0, 4.0)) === Array(4.0, 6.0, -2.0, -2.0))
    assert(fft2c(Array(-7.0, 0.0, 5.0, 13.0)) === Array(
      -2.0, 13.0, -12.0, -13.0))
  }

  def filter_asgns(code: String) =
    code split "\n" filter { _.startsWith("val") } mkString "\n"

  def mkOptFFTDriver() = new FFTStaged with CompileScala
      with ArithExpOptFFT with TrigExpOptFFT {
    self => val codegen =
      new ScalaGenFlat with ScalaGenArrays with ScalaGenArith { // XXX: dirty
        val IR: self.type = self
      }
  }

  test("fft 16 (optimized)") {
    val driver = mkOptFFTDriver
    var fft8c_src: String = ""
    val fft8c = {
      import driver._
      val fft8 = fftc(8)
      val baos = new ByteArrayOutputStream
      codegen.emitSource(fft8, "FFT8", new PrintWriter(baos))
      fft8c_src = baos.toString
      compile(fft8)
    }

    // verify the source does not contain unoptimized patterns
    System.err.println(fft8c_src)
    assert(filter_asgns(fft8c_src).split("\n").size < 100)
    assert(!fft8c_src.contains("1.0*") && !fft8c_src.contains("1.0*"))
    assert(!fft8c_src.contains("-1.0*") && !fft8c_src.contains("*-1.0"))
    assert(!fft8c_src.contains("0.0*") && !fft8c_src.contains("*0.0"))
    assert(!fft8c_src.contains("cos(0.0)") && !fft8c_src.contains("sin(0.0)"))

    // verify the correctness of the optimizations
    assert(fft8c(Array(
      5.0, -7.0,
      0.0, 4.0,
      10.0, -3.0,
      -15.0, 1.0,
      2.0, 2.0,
      11.0, 17.0,
      -5.0, -10.0,
      33.0, 0.0)
    ) === Array(
      41.0, 4.0,
      27.677669529663685, 7.81980515339464,
      22.0, 15.0,
      -38.64823227814083, 57.61879502661796,
      -17.0, -40.0,
      -7.677669529663685, -55.81980515339464,
      -18.0, 1.0,
      30.64823227814083, -45.61879502661796))
  }

}

// Neither of the following aliases doesn't work int the context of
//   self => val codegen = FFTCodegen { val IR: self.type = self }
// type FFTCodegen = ScalaGenFlat with ScalaGenArrays with ScalaGenArith
// trait FFTCodegen extends ScalaGenFlat with ScalaGenArrays with ScalaGenArith
// TODO: How to avoid repeating "new ScalaGenArrays ... with ScalaGenArith" ?

trait ScalaGenArrays extends ScalaGenBase {
  val IR: ArraysExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayApply(x, i) =>
      emitValDef(sym, quote(x) + "(" + i + ")")
    case MakeArray(x) =>
      emitValDef(sym, "Array(" + x.map(quote).mkString(",") + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenArith extends ScalaGenBase {
  val IR: ArithExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Plus(a, b) =>  emitValDef(sym, quote(a) + "+" + quote(b))
    case Minus(a, b) => emitValDef(sym, quote(a) + "-" + quote(b))
    case Times(a, b) => emitValDef(sym, quote(a) + "*" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenFlat extends ScalaGenBase {
   import IR._
   type Block[+T] = Exp[T]

   def getBlockResultFull[T](x: Block[T]): Exp[T] = x
   def reifyBlock[T:Manifest](x: => Exp[T]): Block[T] = x
   def traverseBlock[A](block: Block[A]): Unit = {
     buildScheduleForResult(block) foreach traverseStm
   }
}
