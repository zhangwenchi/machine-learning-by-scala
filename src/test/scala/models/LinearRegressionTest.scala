package models

import org.scalatest.FunSuite

/**
  * 直接做一下效果
  */
class LinearRegressionTest extends FunSuite {

    val x: Seq[Seq[Double]] = Seq(Seq(2.0,10,6,8,10), Seq(2.0,3,3,4,5), Seq(1.0,1.98,2.02,4.001,5.001),
        Seq(1,2,3,8,4.99), Seq(10.0,20.01,30,40,54), Seq(1.2,1,2,3.8,6), Seq(3.01,2.98,3.1,3.99,5.001))
    val y: Seq[Double] = Seq(36.0, 17.0, 14.0, 19.0, 154.0, 14.0, 18.0)

    val x1: Seq[Seq[Double]] = Seq(Seq(1.0,9,6,8,10), Seq(1.0,3,3,4,6), Seq(0.0,0.98,4.02,3.001,5.001),
        Seq(1,2,3,8,5.99), Seq(20.0,20.01,30,40,54), Seq(1.2,1,2,3.9,5), Seq(2.01,3.98,3.1,0.99,8.001))
    val y1: Seq[Double] = Seq(36.0, 17.0, 13.0, 20.0, 164.0, 13.0, 18.0)

    test("LinearRegressionTest.test") {
        val models = new LinearRegression()
        models.fit(x, y, "mape")
        // ArraySeq(0.9141094802164513, 0.9841689724852887, 1.093249296032912, 0.977351600512459, 0.984631493769486, 0.10429043447363284)
        // println(models.w)
        // 2.158E-4
        // println(models.fitError.last)
        val predictions = models.predict(x, y)
        // List(35.998822637751424, 16.997328071358403, 14.009943912188987, 18.998609705768803, 154.00024994110674, 13.993614419848747, 18.00143131201925)
        // println(predictions)
        // 2.175E-5
        // println(models.predictError.last)
        val predictions1 = models.predict(x1, y1)
        // List(34.10054418504969, 17.067850084911438, 13.320812451040611, 19.98324119953829, 163.14134474327125, 13.106718086130508, 18.093330484059166)
        // println(predictions1)
        // 0.63901796
        // println(models.predictError.last)
        // List(2.175E-5, 0.63901796)
        // println(models.predictError)
        assert(predictions1.nonEmpty && predictions.nonEmpty)
    }

}
