package cs590.week4

import scala.virtualization.lms.common._
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import java.io.{ByteArrayOutputStream, PrintWriter}

class StagingTest extends FunSuite with org.scalatest.BeforeAndAfter {

  val eps = 1e-9

  test("power1") {
    val driver = new Power1 with ScalaOpsPkgExp with CompileScala { self =>
      val codegen = new ScalaCodeGenPkg { val IR: self.type = self }
    }

    var src_mults = "" // multiplications in the generated code (for powerX())

    val power4c = {
      import driver._

      val power4 = (x:Rep[Double]) => power(x,4)

      val baos = new ByteArrayOutputStream
      codegen.emitSource(power4, "Power4", new PrintWriter(baos))
      src_mults = baos.toString.split("\n") filter {
        _.contains(" * ")
      } mkString "\n"

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

}
