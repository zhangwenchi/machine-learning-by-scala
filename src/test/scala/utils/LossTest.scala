package utils

import org.scalatest.FunSuite

class LossTest extends FunSuite {
    val fx = Array(1.0, 0.4, 0.2, 0.1, 0.4, 0.6, 0.59999, 0)
    val y = Array(0.8, 0.5, 0.1, 0.2, 0.4, 0.5, 0.6, 0.1)

    test("Loss.squareLoss") {
        assert(Loss.squareLoss(fx, y) == MathUtils.round8(0.09))
    }
}
