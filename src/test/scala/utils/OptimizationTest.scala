package utils

import org.scalatest.FunSuite

class OptimizationTest extends FunSuite {

    val (x1, y1) = (Seq(Seq(1.0, 1, 1), Seq(2.0, 1, 1), Seq(2.0, 2, 1), Seq(2.0, 2, 2)), Seq(15.0, 16, 18, 30))
    val x2 = Array.ofDim[Double](100,100)
    val y2 = 0.0 until 100.0 by 1

    test("Optimization.conjugateGradient") {
        for (i <- 0 until 100) {
            for (j <- 0 until 100) {
                if (i == j) x2(i)(j) = 2
                if (math.abs(i - j) == 1) {
                    x2(i)(j) = 1
                    x2(j)(i) = 1
                }
            }
        }
        println(Optimization.conjugateGradient(x2.toSeq.map(_.toSeq), y2))
    }

    test("Optimization.gradientDescent") {
        println(Optimization.gradientDescent(x1, y1))
    }
}
