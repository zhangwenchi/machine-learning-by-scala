package models

import utils.{Evaluation, MathUtils}

/**
  * 线性回归的实现
  * 1. 基于最小二乘法
  */
class LinearRegression {

    var w: Seq[Double] = Seq()
    var fitError: Double = -1.0
    var predictError: Seq[Double] = Seq()

    def fit(x: Seq[Seq[Double]], y: Seq[Double], errorMethod: String = "mse", method: String = "lsm"): Unit = {
        assert(x.nonEmpty && y.nonEmpty)

        method match {
            case "lsm" => w = MathUtils.leastSquareMethod(x, y)
            case _ => System.err.println("The method is unknown or not implemented.")
        }

        if (w.nonEmpty) {
            val fitResults = fitResult(x)
            fitError = Evaluation.calculateRegressionError(fitResults, y, errorMethod)
        }
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
        x.map(v => (v :+ 1.0).zip(w).foldLeft(0.0)((v1, v2) => v1 + v2._1 * v2._2))
    }
}
