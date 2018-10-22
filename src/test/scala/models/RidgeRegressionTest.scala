package models

import org.scalatest.FunSuite

class RidgeRegressionTest extends FunSuite {

    val x: Seq[Seq[Double]] = Seq(Seq(2.0,10,6,8,10), Seq(2.0,3,3,4,5), Seq(1.0,1.98,2.02,4.001,5.001),
        Seq(1,2,3,8,4.99), Seq(10.0,20.01,30,40,54), Seq(1.2,1,2,3.8,6), Seq(3.01,2.98,3.1,3.99,5.001))
    val y: Seq[Double] = Seq(36.0, 17.0, 14.0, 19.0, 154.0, 14.0, 18.0)

    val x1: Seq[Seq[Double]] = Seq(Seq(1.0,9,6,8,10), Seq(1.0,3,3,4,6), Seq(0.0,0.98,4.02,3.001,5.001),
        Seq(1,2,3,8,5.99), Seq(20.0,20.01,30,40,54), Seq(1.2,1,2,3.9,5), Seq(2.01,3.98,3.1,0.99,8.001))
    val y1: Seq[Double] = Seq(36.0, 17.0, 13.0, 20.0, 164.0, 13.0, 18.0)

    val x2 = Seq(Seq(1.1, 0.1, 1), Seq(1.0, 0.11, 1), Seq(2.0, 0.2, 1), Seq(2.0, 0.2, 2),
                Seq(1.2, 0.1, 1), Seq(1.1, 0.11, 1), Seq(12.0, 0.2, 1), Seq(1.0, 0.2, 2))
    val y2 = Seq(15.0, 15, 18, 21, 15.0, 15, 25, 21)


    test("RidgeRegressionTest.test") {
        val linearRegression = new LinearRegression()
        val ridgeRegression = new RidgeRegression()
        linearRegression.fit(x, y)
        ridgeRegression.fit(x, y, 0.5)

        println("Parameters and fit error.")
        println(linearRegression.w)
        println(ridgeRegression.w)
        println(linearRegression.fitError.last, ridgeRegression.fitError.last)

        val predictionsLR = linearRegression.predict(x, y)
        val predictionsRR = ridgeRegression.predict(x, y)

        println("x, y predict error")
        println(predictionsLR, linearRegression.predictError.last)
        println(predictionsRR, ridgeRegression.predictError.last)

        val predictionsLR2 = linearRegression.predict(x1, y1)
        val predictionsRR2 = ridgeRegression.predict(x1, y1)

        println(predictionsLR2, linearRegression.predictError.last)
        println(predictionsRR2, ridgeRegression.predictError.last)

        linearRegression.fit(x2, y2)
        ridgeRegression.fit(x2, y2, 0.5)
        val predictionsLR3 = linearRegression.predict(x2, y2)
        val predictionsRR3 = ridgeRegression.predict(x2, y2)

        println("Parameters and fit error of biasssss.")
        println(linearRegression.w)
        println(ridgeRegression.w)
        println(linearRegression.fitError.last, ridgeRegression.fitError.last)
        println(predictionsLR3, linearRegression.predictError.last)
        println(predictionsRR3, ridgeRegression.predictError.last)

        assert(predictionsLR.nonEmpty && predictionsLR2.nonEmpty && predictionsLR3.nonEmpty)
    }
}
