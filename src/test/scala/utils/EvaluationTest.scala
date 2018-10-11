package utils

import org.scalatest.FunSuite

class EvaluationTest extends FunSuite {
    val labels = Array(1, 1, 0, 0, 0, 0, 0, 0)
    val predictions = Array(1, 0, 1, 1, 0, 0, 0, 0)
    val predictionsPossibility = Array(0.8, 0.5, 0.1, 0.2, 0.4, 0.5, 0.6, 0.1)

    test("EvaluationTest.accuracy") {
        assert(Evaluation.accuracy(labels, predictions) == MathUtils.round8(5.0 / 8))
    }

    test("EvaluationTest.error") {
        assert(Evaluation.error(labels, predictions) == MathUtils.round8(3.0 / 8))
    }

    test("EvaluationTest.truePositive") {
        assert(Evaluation.truePositive(labels, predictions) == 1)
    }

    test("EvaluationTest.falsePositive") {
        assert(Evaluation.falsePositive(labels, predictions) == 2)
    }

    test("EvaluationTest.trueNegative") {
        assert(Evaluation.trueNegative(labels, predictions) == 4)
    }

    test("EvaluationTest.falseNegative") {
        assert(Evaluation.falseNegative(labels, predictions) == 1)
    }

    test("EvaluationTest.precision") {
        assert(Evaluation.precision(labels, predictions) == MathUtils.round8(1.0 / 3))
    }

    test("EvaluationTest.recall") {
        assert(Evaluation.recall(labels, predictions) == MathUtils.round8(1.0 / 2))
    }

    test("EvaluationTest.precision_recall_curve") {
        assert(Evaluation.precisionRecallCurve(labels, predictionsPossibility).mkString == "(0.5,1.0,0.8)(0.5,0.5,0.6)(1.0,0.5,0.5)(1.0,0.4,0.4)(1.0,0.33333333,0.2)(1.0,0.25,0.1)")
        assert(Evaluation.precisionRecallCurve(Seq(0, 0, 0), Seq(1, 0, 0)).mkString == "(1.0,0.0,1.0)(1.0,0.0,0.0)")
    }

    test("EvaluationTest.fScore") {
        // Evaluation.f_score(labels, predictions, 0)
        assert(Evaluation.fScore(labels, predictions) == 0.4)
        assert(Evaluation.fScore(labels, predictions, 2) == MathUtils.round8(1 / 2.2))
        assert(Evaluation.fScore_(0.6, 0.6) == 0.6)
        assert(Evaluation.fScore_(0.6, 0.5) == MathUtils.round8(0.6 / 1.1))
    }

    test("EvaluationTest.macro") {
        val labels = Seq(Seq(1,0,1,0), Seq(1,0,0,1), Seq(1,0,0,0), Seq(0,0,0,0), Seq(1,1,1,1))
        val predicts = Seq(Seq(1,0,0,0), Seq(0,0,0,0), Seq(1,1,1,1), Seq(0,0,0,0), Seq(1,1,1,1))
        // avg precision (1 + 0 + 1 / 4 + 0 + 1) / 5 | avg recall (1 / 2 + 0 + 1 + 1 + 1) / 5
        val (r, p, f) = Evaluation.macroPRF(labels, predicts)
        assert(p == (1 + 0 + 1.0 / 4 + 0 + 1) / 5)
        assert(r == (1.0 / 2 + 0 + 1 + 1 + 1) / 5)
        assert(f == Evaluation.fScore_(p, r))
    }

    test("EvaluationTest.micro") {
        val labels = Seq(Seq(1,0,1,0), Seq(1,0,0,1), Seq(1,0,0,0), Seq(0,0,0,0), Seq(1,1,1,1))
        val predicts = Seq(Seq(1,0,0,0), Seq(0,0,0,0), Seq(1,1,1,1), Seq(0,0,0,0), Seq(1,1,1,1))
        // avg tp = (1 + 1 + 4) / 5 = 1.2 | avg fp = 3 / 5 = 0.6 | avg fn = (1 + 2) / 5 = 0.6
        val (r, p, f) = Evaluation.microPRF(labels, predicts)
        assert(p == MathUtils.round8(1.2 / (1.2 + 0.6)))
        assert(r == MathUtils.round8(1.2 / (1.2 + 0.6)))
        assert(f == MathUtils.round8(2 * p * r / (p + r)))
    }

    test("EvaluationTest.ROC") {
        assert(Evaluation.ROC(labels, predictionsPossibility).mkString == "(0.0,0.5,0.8)(0.16666667,0.5,0.6)(0.33333333,1.0,0.5)(0.5,1.0,0.4)(0.66666667,1.0,0.2)(1.0,1.0,0.1)")
        assert(Evaluation.ROC(Seq(0, 0, 0), Seq(1.0, 0.5, 0.0)).mkString == "(0.33333333,0.0,1.0)(0.66666667,0.0,0.5)(1.0,0.0,0.0)")
        assert(Evaluation.ROC(Seq(1, 0, 0), Seq(0.0, 0.0, 0.0)).mkString == "(1.0,1.0,0.0)")
        assert(Evaluation.ROC(Seq(0, 0, 0), Seq(0.0, 0.0, 0.0)).mkString == "(1.0,0.0,0.0)")
    }

    test("EvaluationTest.calculateCurveArea") {
        assert(Evaluation.calculateCurveArea(Evaluation.ROC(labels, predictionsPossibility).map(v => (v._1, v._2))) == 0.875)
    }

    test("EvaluationTest.meanSquareError") {
        assert(Evaluation.meanSquareError(Seq(1.0, 2, 3), Seq(1.0, 2, 3)) == 0.0)
        assert(Evaluation.meanSquareError(Seq(1.0, 2, 3), Seq(1.0, 2, 9)) == 12.0)
        assert(Evaluation.meanSquareError(Seq(1.0, 1, 3), Seq(1.0, 2, 4)) == MathUtils.round8(2.0 / 3))
        assert(Evaluation.meanSquareError(Seq(1.0, 2, 3), Seq(1.0, 2.9, 3)) == 0.27)
    }

    test("EvaluationTest.rootMeanSquareError") {
        assert(Evaluation.rootMeanSquareError(Seq(1.0, 2, 3), Seq(1.0, 2, 3)) == 0.0)
        assert(Evaluation.rootMeanSquareError(Seq(1.0, 2, 3), Seq(1.0, 2, 9)) == MathUtils.round8(math.sqrt(12.0)))
        assert(Evaluation.rootMeanSquareError(Seq(1.0, 1, 3), Seq(1.0, 2, 4)) == MathUtils.round8(math.sqrt(2.0 / 3)))
        assert(Evaluation.rootMeanSquareError(Seq(1.0, 2, 3), Seq(1.0, 2.9, 3)) == MathUtils.round8(math.sqrt(0.27)))
    }

    test("EvaluationTest.rSquared") {
        assert(Evaluation.rSquared(Seq(1.0, 2, 3), Seq(1.0, 2, 3)) == 1.0)
        assert(Evaluation.rSquared(Seq(1.0, 2, 3), Seq(2.0, 2, 2)) == 0.0)
        assert(Evaluation.rSquared(Seq(2.5, 0.0, 2, 8), Seq(3, -0.5, 2, 7)) == 0.94860814)
        assert(Evaluation.rSquared(Seq(1.0, 2, 3), Seq(1.0, 2, 9)) == 0.05263158)
        assert(Evaluation.rSquared(Seq(1.0, 1, 3), Seq(1.0, 2, 4)) == 0.57142857)
        assert(Evaluation.rSquared(Seq(1.0, 2, 3), Seq(1.0, 2.9, 3)) == 0.68110236)
    }

    test("EvaluationTest.meanAbsoluteError") {
        assert(Evaluation.meanAbsoluteError(Seq(1.0, 2, 3), Seq(1.0, 2, 3)) == 0.0)
        assert(Evaluation.meanAbsoluteError(Seq(1.0, 2, 3), Seq(1.0, 2, 9)) == 2)
        assert(Evaluation.meanAbsoluteError(Seq(1.0, 1, 3), Seq(1.0, 2, 4.1)) == 0.7)
        assert(Evaluation.meanAbsoluteError(Seq(1.0, 2, 3), Seq(1.0, 2.9, 3)) == 0.3)
    }

    test("EvaluationTest.meanAbsolutePercentError") {
        assert(Evaluation.meanAbsolutePercentError(Seq(1.0, 2, 3), Seq(1.0, 2, 3)) == 0.0)
        assert(Evaluation.meanAbsolutePercentError(Seq(1.0, 2, 3), Seq(1.0, 2, 12)) == 0.25)
        assert(Evaluation.meanAbsolutePercentError(Seq(2.0, 2, 6), Seq(1.0, 1, 3)) == 1)
        assert(Evaluation.meanAbsolutePercentError(Seq(1.0, 2.9, 3), Seq(1.0, 2, 3)) == 0.15)
    }

    test("EvaluationTest.calculateRegressionError") {
        assert(Evaluation.calculateRegressionError(Seq(1.0, 2, 3), Seq(1.0, 2, 3)) == 0.0)
        assert(Evaluation.calculateRegressionError(Seq(1.0, 1, 3), Seq(1.0, 2, 4), "rmse") == MathUtils.round8(math.sqrt(2.0 / 3)))
        assert(Evaluation.calculateRegressionError(Seq(1.0, 1, 3), Seq(1.0, 2, 4), "rs") == 0.57142857)
        assert(Evaluation.calculateRegressionError(Seq(1.0, 1, 3), Seq(1.0, 2, 4.1), "mae") == 0.7)
        assert(Evaluation.calculateRegressionError(Seq(1.0, 2.9, 3), Seq(1.0, 2, 3), "mape") == 0.15)
    }
}
