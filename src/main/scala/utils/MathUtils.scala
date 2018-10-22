package utils

import breeze.linalg.{DenseMatrix, det, inv}

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
      * 最小二乘法的二维计算 [一元一次] (二项式的特殊形式，k,b可以直接根据公式给出)
      * 计算误差一般有两种方法，到直线的距离和与直线y值的差值，一般横轴纵轴没啥关系（计算距离需要用到两个维度的差值，所以一般用后者）
      */
    def leastSquareMethodOneDimensional(x: Seq[Double], y: Seq[Double]): (Double, Double) = {
        assert(x.length == y.length)
        val xMean = x.sum / x.length
        val w = x.zip(y).foldLeft(0.0)((v1, v2) => v1 + v2._2 * (v2._1 - xMean)) /
            (x.foldLeft(0.0)(_ + Math.pow(_, 2)) - 1.0 / x.length * math.pow(x.sum, 2))
        val b = 1.0 / x.length * x.zip(y).foldLeft(0.0)((v1, v2) => v1 + (v2._2 - w * v2._1))
        (w, b)
    }

    /**
      * 一元的最小二乘法的计算 [一元n次]
      * @param poly: including pow(x, 0), 如果是要拟合2次函数则是3
      */
    def leastSquareMethodOneDimensional(x: Seq[Double], y: Seq[Double], poly: Int): Seq[Double] = {
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

    /**
      * 最小二乘法的通用一次计算 [n元一次]
      * 需要用到最小二乘法的矩阵形式
      * W = (XT * X)-1 * XT * y, 其中X是[1, x], XT是转置, -1是逆
      * lambda > 0: 岭回归
      */
    def leastSquareMethod(x: Seq[Seq[Double]], y: Seq[Double], lambda: Double = 0.0): Seq[Double] = {
        assert(x.length == y.length)
        assert(x.head.length < x.length)
        assert(lambda >= 0.0)
        for (i <- x.indices) assert(x.head.length == x(i).length)

        val mX = DenseMatrix(x.map(1.0 +: _): _*)
        val mY = DenseMatrix(y: _*)

        try {
            val paramMatrix = inv(mX.t * mX + lambda * DenseMatrix.eye[Double](mX.cols)) * mX.t * mY
            paramMatrix.toArray.tail :+ paramMatrix.toArray.head
        } catch {
            case _: Throwable =>
                System.err.println("Something wrong in inv in leastSquareMethod !")
                Seq()
        }
    }

    /**
      * 最小二乘法的通用n次计算 [n元n次]
      */
    def leastSquarePolyMethod(x: Seq[Seq[Double]], y: Seq[Double], poly:Int): Seq[Double] = {
        assert(x.length == y.length)
        assert(x.head.length < x.length)
        assert(x.head.length * poly < x.length)
        for (i <- x.indices) assert(x.head.length == x(i).length)

        val processX = x.map(f => f.flatMap(v => {var y = Seq[Double](); for (i <- 1 to poly) y :+= math.pow(v, i); y.reverse}))

        leastSquareMethod(processX, y)
    }

    def sigmoid(z: Double): Double = {
        round8(1.0 / (1.0 + math.pow(math.E, -z)))
    }

    /**
      * TODO
      * LSQR: 迭代找最小二乘，需要避免某一列 / 某一行的scale和别人不一样 （每一列最好归一化）
      *
      * Lanczos 方法： 将对称矩阵通过正交相似变换成对称三对角矩阵的算法 【求一个矩阵的正交基】，这个算法能保留矩阵的稀疏性
      * 考虑方程 B * x = b, V = {v1, .. , vm} 是n维空间中m个无关向量，想找 (B * x - b) 正交 vi 的解，
      * 令 xm = Vm * ym，其中ym为m*1的实向量，则有
      * (Vm.T * B * Vm)ym = Vm.T * b
      * 假设上式有唯一解，则通过解这个可以得到ym，从而得到xm，
      * Lanczos有一套构造向量 {v1, .., vm} 的方法，使得矩阵 Vm.T * B * Vm 具有三对角形式
      * v1 = b / β1, β1 = ||b||, v0 = 0,
      * for i = 1, 2, ..
      *     wi = B * vi - βi * vi-1
      *     αi = vi.T * wi
      *     βi+1 = ||vi+1||, 不能存在这样的βi+1则停止
      *     vi+1 = (wi - αi * vi) / βi+1
      * 等价于: B * Vm = Vm * Tm + βm+1(0, .., 0, vm+1), 其中 Tm = ([α1, β1, 0, .., 0], [β2, α2, β3, .., 0])
      * Vm.T * B * Vm = Vm.T * (Vm * Tm + βm+1(0, .., 0, vm+1)) = Vm.T * Vm * Tm + βm+1 * Vm.T * (0, .., 0, vm+1)
      * => Tm * ym = Vm.T * b = β1 * (1, 0, .., 0).T
      */
}
