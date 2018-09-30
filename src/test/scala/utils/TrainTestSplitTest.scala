package utils

import org.scalatest.FunSuite

class TrainTestSplitTest extends FunSuite {
    val features: Seq[Seq[Any]] = Seq(
        Seq(1, 1, 1.0, 4.0, 5.5, "b", 2.3, 100, 2.5, 0), Seq(11, 2, 2.0, 4.0, 5.5, "a", 2.3, 100, 2.5, 0),
        Seq(2, 1, 1.0, 4.0, 5.5, "d", 2.3, 100, 2.5, 1), Seq(12, 2, 2.0, 4.0, 5.5, "a", 2.3, 100, 2.5, 0),
        Seq(3, 3, 2.0, 4.0, 5.5, "f", 2.3, 100, 2.5, 1), Seq(13, 7, 3.0, 4.0, 5.5, "a", 2.3, 100, 2.5, 0),
        Seq(4, 3, 1.1, 4.0, 5.5, "f", 2.3, 100, 2.5, 1), Seq(14, 7, 4.0, 4.0, 5.5, "a", 2.3, 100, 2.5, 0),
        Seq(5, 5, 1.3, 4.0, 5.5, "f", 2.3, 100, 2.5, 1), Seq(15, 5, 5.0, 4.0, 5.5, "s", 2.3, 99, 2.5, 0),
        Seq(6, 8, 1.4, 4.0, 5.5, "b", 2.3, 100, 2.5, 1), Seq(16, 9, 6.0, 4.0, 5.5, "d", 2.3, 100, 2.5, 0),
        Seq(7, 8, 1.5, 4.0, 5.5, "b", 2.3, 100, 2.5, 1), Seq(17, 9, 7.0, 4.0, 5.5, "f", 2.3, 100, 2.5, 0),
        Seq(8, 10, 1.6, 4.0, 5.5, "b", 2.3, 100, 2.5, 1), Seq(18, 10, 8.0, 4.0, 5.5, "g", 2.3, 100, 2.5, 0),
        Seq(9, 6, 1.7, 4.0, 5.5, "b", 2.3, 100, 2.5, 1), Seq(19, 4, 9.0, 4.0, 5.5, "g", 2.3, 100, 2.5, 0),
        Seq(10, 6, 1.8, 4.0, 5.5, "b", 2.3, 100, 2.5, 1), Seq(20, 4, 0.0, 4.0, 5.5, "g", 2.3, 100, 2.5, 1)
    )
    val labels: Seq[Int] = Seq(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    )
    test("TrainTestSplit.trainTestSplit") {
        var xTrain: Seq[Seq[Any]] = Seq()
        var yTrain: Seq[Int] = Seq()
        var xTest: Seq[Seq[Any]] = Seq()
        var yTest: Seq[Int] = Seq()

        val x1 = TrainTestSplit.trainTestSplit(features)
        xTrain = x1._1
        yTrain = x1._2
        xTest = x1._3
        yTest = x1._4
        assert(xTrain.length == 14)
        assert(xTest.length == 6)
        assert(yTest.count(_ == 1) == yTest.count(_ == 0))
        assert(yTrain.count(_ == 1) == yTrain.count(_ == 0))
        xTrain.zip(yTrain).map(v => v._1 :+ v._2).foreach(v => assert(features.map(_.mkString).contains(v.mkString)))
        xTest.zip(yTest).map(v => v._1 :+ v._2).foreach(v => assert(features.map(_.mkString).contains(v.mkString)))
        assert(xTrain.map(_.head.asInstanceOf[Int]).sum + xTest.map(_.head.asInstanceOf[Int]).sum == (1 + 20) * 20 / 2)
        assert(yTrain.++(yTest).apply(xTrain.++(xTest).indexWhere(_.head == 1)) == 0)

        val x2 = TrainTestSplit.trainTestSplit(features, labels)
        xTrain = x2._1
        yTrain = x2._2
        xTest = x2._3
        yTest = x2._4
        assert(xTrain.length == 14)
        assert(xTest.length == 6)
        assert(yTest.count(_ == 1) == yTest.count(_ == 0))
        assert(yTrain.count(_ == 1) == yTrain.count(_ == 0))
        xTrain.zip(yTrain).map(v => v._1 :+ v._2).foreach(v => assert(features.zip(labels).map(v => v._1 :+ v._2).map(_.mkString).contains(v.mkString)))
        xTest.zip(yTest).map(v => v._1 :+ v._2).foreach(v => assert(features.zip(labels).map(v => v._1 :+ v._2).map(_.mkString).contains(v.mkString)))
        assert(xTrain.map(_.head.asInstanceOf[Int]).sum + xTest.map(_.head.asInstanceOf[Int]).sum == (1 + 20) * 20 / 2)
        assert(yTrain.++(yTest).apply(xTrain.++(xTest).indexWhere(_.head == 1)) == 1)

        // not easily test randomly things, print is a good way
        val x3 = TrainTestSplit.trainTestSplit(features, shuffledCol=1)
        xTrain = x3._1
        yTrain = x3._2
        xTest = x3._3
        yTest = x3._4
        xTrain.zip(yTrain).map(v => v._1 :+ v._2).foreach(v => assert(features.map(_.mkString).contains(v.mkString)))
        xTest.zip(yTest).map(v => v._1 :+ v._2).foreach(v => assert(features.map(_.mkString).contains(v.mkString)))
        assert(xTrain.map(_.head.asInstanceOf[Int]).sum + xTest.map(_.head.asInstanceOf[Int]).sum == (1 + 20) * 20 / 2)
        assert(yTrain.++(yTest).apply(xTrain.++(xTest).indexWhere(_.head == 1)) == 0)
        assert(xTrain.map(_(1)).toSet.size > 5)
        assert(xTest.map(_(1)).toSet.size < 5)
//        println(xTrain.map(_(1).asInstanceOf[Int]).sorted.mkString(", "))
    }

    test("TrainTestSplit.trainValidateTestSplit") {
        var xTrain: Seq[Seq[Any]] = Seq()
        var yTrain: Seq[Int] = Seq()
        var xValidate: Seq[Seq[Any]] = Seq()
        var yValidate: Seq[Int] = Seq()
        var xTest: Seq[Seq[Any]] = Seq()
        var yTest: Seq[Int] = Seq()

        val x1 = TrainTestSplit.trainValidateTestSplit(features)
        xTrain = x1._1
        yTrain = x1._2
        xValidate = x1._3
        yValidate = x1._4
        xTest = x1._5
        yTest = x1._6

        // println(xTest.map(_.mkString(", ")).mkString("\n"))
        assert(xTrain.length == 14)
        assert(xValidate.length == 2)
        assert(xTest.length == 4)
        assert(yTest.count(_ == 1) == yTest.count(_ == 0))
        assert(yTrain.count(_ == 1) == yTrain.count(_ == 0))
        xTrain.zip(yTrain).map(v => v._1 :+ v._2).foreach(v => assert(features.map(_.mkString).contains(v.mkString)))
        xTest.zip(yTest).map(v => v._1 :+ v._2).foreach(v => assert(features.map(_.mkString).contains(v.mkString)))
        assert(xTrain.map(_.head.asInstanceOf[Int]).sum + xValidate.map(_.head.asInstanceOf[Int]).sum + xTest.map(_.head.asInstanceOf[Int]).sum == (1 + 20) * 20 / 2)
        assert(yTrain.++(yTest).++(xValidate).apply(xTrain.++(xTest).++(xValidate).indexWhere(_.head == 1)) == 0)
    }

    test("TrainTestSplit.kFoldSplit") {
        var xTrain: Seq[Seq[Any]] = Seq()
        var yTrain: Seq[Int] = Seq()
        var xTest: Seq[Seq[Any]] = Seq()
        var yTest: Seq[Int] = Seq()

        val x1 = TrainTestSplit.kFoldSplit(features)
        xTrain = x1._1
        yTrain = x1._2
        xTest = x1._3
        yTest = x1._4
        assert(xTrain.length == 18)
        assert(xTest.length == 2)
    }

    test("TrainTestSplit.bootStrapping") {
        val (x, y) = TrainTestSplit.bootStrapping(features)
        assert(x.length == features.length)
        // println(x.map(_.mkString).toSet.size)
    }
}
