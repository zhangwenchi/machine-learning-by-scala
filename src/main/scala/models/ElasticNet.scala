package models

import breeze.linalg.DenseMatrix
import utils.{Evaluation, MathUtils, NormalizationMethod, NormalizationType}
import utils.NormalizationType.NormalizationType
import utils.Optimization.FrobeniusNorm

/**
  * ElasticNet is a linear regression model trained with L1 and L2 prior as regularizer.
  * This combination allows for learning a sparse model where few of the weights are non-zero like Lasso,
  * while still maintaining the regularization properties of Ridge.
  */
class ElasticNet {
    var w: Seq[Double] = Seq()
    var b: Double = 0.0
    var predictError: Seq[Double] = Seq()
    var fitError: Seq[Double] = Seq()

    def fit(x: Seq[Seq[Double]], y: Seq[Double], errorMethod: String, learningRate: Double = 0.1, alpha: Double = 0.1, lRatio: Double = 0.5, tol: Double = 1e-6, maxIter: Int = 1000, normMethod: NormalizationType = NormalizationType.Max): Unit = {
        if (x.length != y.length) throw new IllegalArgumentException("x, y of ElasticNet should have the same length.")
        if (lRatio < 0.0) throw new IllegalArgumentException("lRatio of ElasticNet should be larger than 0.")
        if (tol < 0) throw new IllegalArgumentException("Tolerance of ElasticNet should be larger than 0.")
        if (maxIter <= 0) throw new IllegalArgumentException("maxIteration of ElasticNet should be larger than 0.")

        val xNorm = NormalizationMethod.normalizeByColumn(x, normMethod)

        train(xNorm, y, learningRate, alpha, lRatio, tol, maxIter)

        val maxValue = x.transpose.map(_.max)
        w = w.zip(maxValue).map(v => v._1 / v._2).map(MathUtils.round8)

        b = y.sum / y.length - w.zip(MathUtils.colsMean(x)).map(v => v._1 * v._2).sum

        if (w.nonEmpty) {
            val fitResults = fitResult(x)
            fitError :+= Evaluation.calculateRegressionError(fitResults, y, errorMethod)
        }
    }

    def train(x: Seq[Seq[Double]], y: Seq[Double], learningRate: Double, alpha: Double, lRatio: Double, tol: Double, maxIter: Int): Unit = {
        val A = DenseMatrix(x: _*)
        val b = DenseMatrix(y: _*)
        val c = DenseMatrix((1 to x.head.length).map(_ => (new util.Random).nextGaussian()): _*)
        var iteration= 0
        while (iteration < maxIter &&  FrobeniusNorm(A * c - b) / FrobeniusNorm(b) >= tol) {
            iteration += 1
            c -= learningRate / A.cols * A.t * (A * c - b) + learningRate * lRatio * l1Dif(c, alpha) + learningRate * (1 - lRatio) * c
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

    def l1Dif(W: DenseMatrix[Double], lambda: Double): DenseMatrix[Double] = {
        W.map(v => if (v > 0) 1 else if (v < 0) -1)
        lambda * W
    }
}