package utils

object Loss {

    def squareLoss(fx: Seq[Double], y: Seq[Double]): Double = {
        assert(fx.length == y.length)
        MathUtils.round8(fx.zip(y).map(v => math.pow(v._1 - v._2, 2)).sum)
    }

    def rootSquareLoss(fx: Seq[Double], y: Seq[Double]): Double = {
        assert(fx.length == y.length)
        MathUtils.round8(math.sqrt(fx.zip(y).map(v => math.pow(v._1 - v._2, 2)).sum))
    }

}
