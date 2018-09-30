package utils

object MathUtils {

    def round8(value: Double): Double = try {
        "%.8f".format(value).toDouble
    } catch {
        case _: Throwable => value
    }

    def mean(value: Seq[Double]): Double = {
        if (value.nonEmpty) value.sum / value.length else 0.0
    }

    def std(value: Seq[Double]): Double = {
        val avg = mean(value)
        if (value.nonEmpty) math.sqrt(value.map(x => math.pow(x - avg, 2)).sum / value.length) else -1
    }

    /**
      * 最小二乘法的二维计算 [一元一次] (二项式的特殊形式，k,b可以直接根据公式给出)
      * 最小二乘法的矩阵运算[多元多次](如n个特征)涉及到Jacobian matrix矩阵(由 xn/ym 的偏导组成，描述了最佳逼近)和矩阵的转置和逆，比较复杂
      */
    def leastSquareMethodLinear(x: Seq[Double], y: Seq[Double]): (Double, Double) = {
        assert(x.length == y.length)
        val xMean = x.sum / x.length
        val w = x.zip(y).foldLeft(0.0)((v1, v2) => v1 + v2._2 * (v2._1 - xMean)) /
            (x.foldLeft(0.0)(_ + Math.pow(_, 2)) - 1.0 / x.length * math.pow(x.sum, 2))
        val b = 1.0 / x.length * x.zip(y).foldLeft(0.0)((v1, v2) => v1 + (v2._2 - w * v2._1))
        (w, b)
    }

    /**
      * 增广矩阵(方程组)的计算
      */
    def augmentedMatrixCalculation(matrix: Array[Array[Double]]): Seq[Double] = {
        assert(matrix.nonEmpty)
        matrix.foreach(v => assert(v.length == matrix.length + 1))

        val poly = matrix.length

        val result: Array[Double] = new Array[Double](poly)

        // 消元法，将左边poly * poly的矩阵变成右上三角矩阵
        for (k <- 0 until poly - 1) {
            for (i <- k + 1 until poly) {
                val coefficient = if (matrix(k)(k) != 0) matrix(i)(k) / matrix(k)(k) else 0
                for (j <- k until poly + 1) matrix(i)(j) = matrix(i)(j) - matrix(k)(j) * coefficient
            }
        }

        // 每一行n应该只剩下matrix(n)(n)和matrix(n)(poly)，他俩中间的那些都应该被下面已经求得的系数给消掉，
        // 这个是计算如果把其余的都消掉，最后一列(y)应该减掉多少[对于matrix(n)(n)他下面啥也没有所以不影响]
        def func(l: Int, r: Int): Double = {
            var sum: Double = 0.0
            for (i <- l to r) sum += matrix(l - 1)(i) * result(i)
            sum
        }

        //ploy次幂项的系数一目了然
        result(poly - 1) = matrix(poly - 1)(poly) / matrix(poly - 1)(poly - 1)
        // 然后一个一个往上求解 (回代求解)
        for (i <- poly - 2 to 0 by -1) result(i) = (matrix(i)(poly) - func(i + 1, poly - 1)) / matrix(i)(i)
        result
    }

    /**
      * 最小二乘法的二维计算 [一元n次]
      * @param poly: including pow(x, 0), 如果是要拟合2次函数则是3
      */
    def leastSquareMethod(x: Seq[Double], y: Seq[Double])(poly: Int): Seq[Double] = {
        assert(x.length > poly)
        assert(x.length == y.length)
        assert(poly > 1)

        // 方程组的增广矩阵 (对任意一个系数求偏导，可以得到poly个等式)
        val valueMatrix = Array.ofDim[Double](poly, poly + 1)
        for (i <- 0 until poly) {
            for (j <- 0 until poly) valueMatrix(i)(j) = x.foldLeft(0.0)((v1, v2) => v1 + math.pow(v2, i + j))
            valueMatrix(i)(poly) = x.zip(y).foldLeft(0.0)((v1, v2) => v1 + math.pow(v2._1, i) * v2._2)
        }
        augmentedMatrixCalculation(valueMatrix).reverse
    }

}
