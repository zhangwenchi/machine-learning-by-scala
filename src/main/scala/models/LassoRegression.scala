package models

import breeze.linalg.DenseMatrix
import utils.NormalizationType.NormalizationType
import utils.{Evaluation, MathUtils, NormalizationMethod, NormalizationType}
import utils.Optimization.FrobeniusNorm

/**
  * least absolute shrinkage and selection operator
  * 借用l1正则化可得稀疏解
  * The objective function in the L1-regularized least squares is convex but not differentiable 凸但是不可微分
  */
class LassoRegression {

    var w: Seq[Double] = Seq()
    var b: Double = 0.0
    var predictError: Seq[Double] = Seq()
    var fitError: Seq[Double] = Seq()

    def fit(x: Seq[Seq[Double]], y: Seq[Double], errorMethod: String, alpha: Double = 0.05, lambda: Double = 0.001, tol: Double = 1e-6, maxIter: Int = 1000, normMethod: NormalizationType = NormalizationType.Max): Unit = {
        if (x.length != y.length) throw new IllegalArgumentException("x, y of LASSO should have the same length.")
        if (lambda < 0.0) throw new IllegalArgumentException("Lambda of LASSO should be larger than 0.")
        if (tol < 0) throw new IllegalArgumentException("Tolerance of LASSO should be larger than 0.")
        if (maxIter <= 0) throw new IllegalArgumentException("maxIteration of LASSO should be larger than 0.")

        val xNorm = NormalizationMethod.normalizeByColumn(x, normMethod)

        train(xNorm, y, alpha, lambda, tol, maxIter)

        val maxValue = x.transpose.map(_.max)
        w = w.zip(maxValue).map(v => v._1 / v._2).map(MathUtils.round8)

        b = y.sum / y.length - w.zip(MathUtils.colsMean(x)).map(v => v._1 * v._2).sum

        if (w.nonEmpty) {
            val fitResults = fitResult(x)
            fitError :+= Evaluation.calculateRegressionError(fitResults, y, errorMethod)
        }
    }

    def train(x: Seq[Seq[Double]], y: Seq[Double], alpha: Double, lambda: Double, tol: Double, maxIter: Int): Unit = {
        val A = DenseMatrix(x: _*)
        val b = DenseMatrix(y: _*)
        val c = DenseMatrix((1 to x.head.length).map(_ => (new util.Random).nextGaussian()): _*)
        var iteration= 0
        while (iteration < maxIter &&  FrobeniusNorm(A * c - b) / FrobeniusNorm(b) >= tol) {
            iteration += 1
            c -= alpha / A.cols * A.t * (A * c - b) + l1Dif(c, lambda)
        }
        w = c.toArray
    }

    def predict(x: Seq[Seq[Double]], y: Seq[Double] = Seq(), errorMethod: String = "mse"): Seq[Double] = {
        if (w.isEmpty) {
            System.err.println("Please fit first.")
            Seq()
        } else {
            val predictions = fitResult(x)
            if (y.nonEmpty) predictError :+= Evaluation.calculateRegressionError(predictions, y)
            predictions
        }
    }

    def fitResult(x: Seq[Seq[Double]]): Seq[Double] = {
        x.map(v => v.zip(w).map(v => v._1 * v._2).sum + b)
    }

    /**
      * l1正则项求导，等于0的时候直接忽略掉了。。。。
      * @param W 系数
      * @param lambda l1系数
      * @return 系数
      */
    def l1Dif(W: DenseMatrix[Double], lambda: Double): DenseMatrix[Double] = {
        W.map(v => if (v > 0) 1 else if (v < 0) -1)
        lambda * W
    }
}
