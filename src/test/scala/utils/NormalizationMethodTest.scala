package utils

import org.scalatest.FunSuite

class NormalizationMethodTest extends FunSuite {
    val x = Seq(1.0, 3,4,5,2,5,6,7,7,8,8)
    val x1 = Seq(Seq(1.0, 2, 3), Seq(2.0, 3, 5))

    test("NormalizationMethodTest.normalize") {
        assert(NormalizationMethod.normalize(x, NormalizationType.MinMax) == Seq(0.0, 0.2857142857142857, 0.42857142857142855, 0.5714285714285714, 0.14285714285714285, 0.5714285714285714, 0.7142857142857143, 0.8571428571428571, 0.8571428571428571, 1.0, 1.0))
        assert(NormalizationMethod.normalize(Seq(), NormalizationType.MinMax) == Seq())
        assert(NormalizationMethod.normalize(Seq(2.0, 2.0), NormalizationType.MinMax) == Seq(1.0, 1.0))

        assert(NormalizationMethod.normalize(x, NormalizationType.ZScore) == Seq(-1.7985617256992208, -0.9192648820240462, -0.47961646018645887, -0.03996803834887154, -1.3589133038616334, -0.03996803834887154, 0.3996803834887158, 0.8393288053263032, 0.8393288053263032, 1.2789772271638904, 1.2789772271638904))
        assert(NormalizationMethod.normalize(Seq(), NormalizationType.ZScore) == Seq())
        assert(NormalizationMethod.normalize(Seq(2.0, 2.0), NormalizationType.ZScore) == Seq(1.0, 1.0))

        assert(NormalizationMethod.normalizeByRow(x1, NormalizationType.MinMax) == Seq(Seq(0.0, 0.5, 1.0), Seq(0.0, 0.3333333333333333, 1.0)))
        assert(NormalizationMethod.normalizeByColumn(x1, NormalizationType.MinMax) == Seq(Seq(0.0, 0.0, 0.0), Seq(1.0, 1.0, 1.0)))
    }
}
