package models

import org.scalatest.FunSuite

class ElasticNetTest extends FunSuite {

    val x: Seq[Seq[Double]] = Seq(Seq(2.0,10,6,8,10), Seq(2.0,3,3,4,5), Seq(1.0,1.98,2.02,4.001,5.001),
        Seq(1,2,3,8,4.99), Seq(10.0,20.01,30,40,54), Seq(1.2,1,2,3.8,6), Seq(3.01,2.98,3.1,3.99,5.001))
    val y: Seq[Double] = Seq(36.0, 17.0, 14.0, 19.0, 154.0, 14.0, 18.0)

    val (x2, y2) = (Seq(Seq(1.0, 1, 1), Seq(2.0, 1, 1), Seq(2.0, 2, 1), Seq(2.0, 2, 2)), Seq(15.0, 16, 18, 30))

    val x1: Seq[Seq[Double]] = Seq(Seq(1.0,9,6,8,10), Seq(1.0,3,3,4,6), Seq(0.0,0.98,4.02,3.001,5.001),
        Seq(1,2,3,8,5.99), Seq(20.0,20.01,30,40,54), Seq(1.2,1,2,3.9,5), Seq(2.01,3.98,3.1,0.99,8.001))
    val y1: Seq[Double] = Seq(36.0, 17.0, 13.0, 20.0, 164.0, 13.0, 18.0)

    test("ElasticNetTest.test") {
        val models = new ElasticNet()
        models.fit(x2, y2, "mape",lRatio = 1)
        models.predict(x2, y2)
        println(models.w)
        println(models.fitResult(x2).mkString(", "))
        models.fit(x1, y1, "mape",lRatio = 1)
        val predictions1 = models.predict(x1, y1)
        println(models.w)
        println(models.fitResult(x1).mkString(", "))
        models.fit(x, y, "mape",lRatio = 1)
        println(models.w)
        println(models.fitResult(x).mkString(", "))
        val predictions = models.predict(x, y)

        println(models.predictError.mkString(", "))
        assert(predictions1.nonEmpty && predictions.nonEmpty)
    }

}