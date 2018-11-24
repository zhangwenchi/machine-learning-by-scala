package utils

import breeze.linalg.{DenseMatrix, trace}
import utils.NormalizationType.NormalizationType

object Optimization {

    // srqt(trace(A.T * A))
    def FrobeniusNorm(A: DenseMatrix[Double]): Double = {
        math.sqrt(trace(A.t * A))
    }

    def fitResult(beta: Seq[Double], x: Seq[Seq[Double]]): Seq[Double] = {
        x.map(v => v.zip(beta).map(t => t._1 * t._2).sum)
    }

    /**
      * 梯度下降法
      * @return 系数
      */
    def gradientDescent(x: Seq[Seq[Double]], y: Seq[Double], maxIteration: Int = 10000, tol: Double = 1e-8,
                        normMethod: NormalizationType = NormalizationType.Max, alpha: Double = 0.1): Seq[Double] = {
        val xNorm = NormalizationMethod.normalizeByColumn(x.map(1.0 +: _), normMethod)

        var i = 0
        var beta: Seq[Double] = List.fill(xNorm.head.length)(1.0)

        while (i < maxIteration && Evaluation.calculateRegressionError(fitResult(beta, xNorm), y, "rmse") > tol) {
            beta = updateBeta(beta, computeGradient(xNorm, y, beta), alpha)
            i += 1
        }

        val maxValue = 1.0 +: x.transpose.map(_.max)
        beta.zip(maxValue).map(v => v._1 / v._2).map(MathUtils.round8)
    }

    /**
      * 随机梯度下降法
      */
    def stochasticGradientDescent(x: Seq[Seq[Double]], y: Seq[Double], maxIteration: Int = 100, tol: Double = 1e-8,
                        normMethod: NormalizationType = NormalizationType.Max, alpha: Double = 0.1): Seq[Double] = {
        miniBatchGradientDescent(x, y, maxIteration, tol, normMethod, alpha, 1)
    }

    /**
      * 批量梯度下降
      */
    def miniBatchGradientDescent(x: Seq[Seq[Double]], y: Seq[Double], maxIteration: Int = 100, tol: Double = 1e-8,
                                 normMethod: NormalizationType = NormalizationType.Max, alpha: Double = 0.1, batch: Int = 2): Seq[Double] = {
        val xNorm = NormalizationMethod.normalizeByColumn(x.map(1.0 +: _), normMethod)

        var i = 0
        var beta: Seq[Double] = List.fill(xNorm.head.length)(1.0)

        while (i < maxIteration && Evaluation.calculateRegressionError(fitResult(beta, xNorm), y, "rmse") > tol) {
            var j = 0
            while (j <= xNorm.length - batch && Evaluation.calculateRegressionError(fitResult(beta, xNorm), y, "rmse") > tol) {
                beta = updateBeta(beta, computeGradient(xNorm.slice(j, j + batch), y.slice(j, j + batch), beta), alpha)
                j += batch
            }
            j = y.length
            while (j >= batch && Evaluation.calculateRegressionError(fitResult(beta, xNorm), y, "rmse") > tol) {
                beta = updateBeta(beta, computeGradient(xNorm.slice(j - batch, j), y.slice(j - batch, j), beta), alpha)
                j -= batch
            }
            i += 1
        }

        val maxValue = 1.0 +: x.transpose.map(_.max)
        beta.zip(maxValue).map(v => v._1 / v._2).map(MathUtils.round8)
    }

    /**
      * 共轭梯度下降法
      * @return c: 系数矩阵
      */
    def conjugateGradient(x: Seq[Seq[Double]], y: Seq[Double], maxIteration: Int = 10000, tol: Double = 1e-6): Seq[Double] = {
        val A = DenseMatrix(x: _*)
        val b = DenseMatrix(y: _*)
        // val c: Seq[Double] = Seq.fill(x.head.length)(1)
        val c = DenseMatrix((1 to x.head.length).map(_ => 1.0): _*)

        var r = b - A * c
        var p = r
        var iteration = 0

        while (iteration < maxIteration && FrobeniusNorm(A * c - b) / FrobeniusNorm(b) >= tol) {
            iteration += 1
            val rRaw = r
            val alpha = (r.t * r) / (p.t * A * p)
            c += (alpha * p.t).t
            r = b - A * c
            val beta = math.pow(FrobeniusNorm(r), 2) / math.pow(FrobeniusNorm(rRaw), 2)
            p = r + beta * p
        }
        c.toArray
    }

    def computeGradient(x: Seq[Seq[Double]], y: Seq[Double], beta: Seq[Double]): Seq[Double] = {
        val base = new Array[Double](y.length)
        for (j <- y.indices) {
            for (i <- beta.indices) base(j) += beta(i) * x(j)(i)
        }

        for (i <- base.indices) base(i) -= y(i)

        val gradient = new Array[Double](x.head.length)
        for (i <- beta.indices) {
            for (j <- y.indices) gradient(i) += x(j)(i) * base(j)
        }
        gradient.map(v => v / y.length * 2)
    }

    def updateBeta(beta: Seq[Double], gradient: Seq[Double], alpha: Double): Seq[Double] = {
        beta.zip(gradient).map(v => v._1 - alpha * v._2)
    }
}
