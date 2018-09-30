package utils

import org.scalatest.FunSuite

class MathUtilsTest extends FunSuite {

    test("MathUtilsTest.round8") {
        assert(MathUtils.round8(21.000000000000012) == 21.0)
        assert(MathUtils.round8(2123) == 2123)
        assert(MathUtils.round8(21.123456785) == 21.12345679)
        assert(MathUtils.round8(21.123456784) == 21.12345678)
    }

    test("MathUtilsTest.mean") {
        assert(MathUtils.mean(Seq(1,2,3)) == 2)
        assert(MathUtils.mean(Seq(1.2,2.5,3.7)) == 7.4 / 3)
        assert(MathUtils.mean(Seq()) == 0)
    }

    test("MathUtilsTest.std") {
        assert(MathUtils.std(Seq(1,2,3)) == math.sqrt(2.0 / 3))
        assert(MathUtils.std(Seq(0,0,0)) == 0)
        assert(MathUtils.std(Seq()) == -1)
    }

    test("MathUtilsTest.leastSquareMethodLinear") {
        assert(MathUtils.leastSquareMethodLinear(Seq(1,2,3,4,5), Seq(1,2,3,4,5)) == (1, 0))
        assert(MathUtils.leastSquareMethodLinear(Seq(1,2,3,4,5), Seq(2,4,6,8,10)) == (2, 0))
        assert(MathUtils.leastSquareMethodLinear(Seq(1,2,3,4,5), Seq(2+1,4+1,6+1,8+1,10+1)) == (2, 1))
    }

    test("MathUtilsTest.augmentedMatrixCalculation") {
        val matrix1 = Array(Array(1.0, 2, 3, 0), Array(3.0, 4, 7, 2), Array(6.0, 5, 9, 11))
        assert(MathUtils.augmentedMatrixCalculation(matrix1).mkString == Array(4.0, 1.0, -2.0).mkString)
        val matrix2 = Array(Array(2.0, -1, 3, -2), Array(1.0, 1, -1, 8), Array(3.0, -9, -2, 9))
        assert(MathUtils.augmentedMatrixCalculation(matrix2).mkString == Array(4.0, 1.0, -3.0).mkString)
    }

    test("MathUtilsTest.leastSquareMethod") {
        val (x1, y1) = (Seq(1.0, 2, 3, 4, 5, 6), Seq(1.0, 4, 9, 16, 25, 36))
        assert(MathUtils.leastSquareMethod(x1, y1)(3) == Seq(1.0, 0, 0))
        val (x2, y2) = (Seq(1.0, 2, 3, 4, 5, 6), Seq(2.0, 5, 10, 17, 26, 37))
        assert(MathUtils.leastSquareMethod(x2, y2)(3) == Seq(1.0, 0, 1))
        val (x3, y3) = (Seq(1.0, 2, 3, 4, 5, 6), Seq(3.0, 7, 13, 21, 31, 43))
        assert(MathUtils.leastSquareMethod(x3, y3)(3) == Seq(1.0, 1, 1))
//        val (x4, y4) = (Seq(1.0, 2, 3, 4, 5, 6), Seq(3.0, 12, 24, 88, 125, 177))
//        println(MathUtils.leastSquareMethod(x4, y4)(4))
    }
}
