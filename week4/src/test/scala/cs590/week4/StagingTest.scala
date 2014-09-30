package cs590.week4

import scala.virtualization.lms.common._
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import java.io.{ByteArrayOutputStream, PrintWriter}

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
  }

}
