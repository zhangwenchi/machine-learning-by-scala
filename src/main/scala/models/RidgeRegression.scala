package models

import utils.{Evaluation, MathUtils}

class RidgeRegression extends LinearRegression {

    override def fit(x: Seq[Seq[Double]], y: Seq[Double]): Unit = {
        System.err.println("Please set lambda for ridge regression!")
        super.fit(x, y)
    }

    def fit(x: Seq[Seq[Double]], y: Seq[Double], lambda: Double, errorMethod: String = "mse", method: String = "lsm"): Unit = {
        assert(x.nonEmpty && y.nonEmpty)

        method match {
            case "lsm" => w = MathUtils.leastSquareMethod(x, y, lambda)
            case _ => System.err.println("The method is unknown or not implemented.")
        }

        if (w.nonEmpty) {
            val fitResults = fitResult(x)
            fitError :+= Evaluation.calculateRegressionError(fitResults, y, errorMethod)
        }
    }

}
