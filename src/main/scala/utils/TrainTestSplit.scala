package utils

import scala.util.Random

/**
  * 留出法 单纯的spilt
  * 交叉验证法： 分为几份分别作为测试集
  * 自助法：自助采样， 约 1/3 测试集没有出现在训练集 out-of-bag estimate (数据量较小时会用，但是由于有放回采样，改变了分布会引入估计偏差)
  * 注意如果features是有归属的同一归属的样本需要考虑是否同时放到测试集和训练集中
 */

object TrainTestSplit {

    def trainValidateTestSplit(features: Seq[Seq[Any]], labels: Seq[Int] = Seq(), splitRatio: (Double, Double) = (0.7, 0.1), shuffledCol: Int = -1):
    (Seq[Seq[Any]], Seq[Int], Seq[Seq[Any]], Seq[Int], Seq[Seq[Any]], Seq[Int]) = {
        val (xTrainValidate, yTrainValidate, xTest, yTest) = trainTestSplit(features, labels, splitRatio._1 + splitRatio._2, shuffledCol)
        val (xTrain, yTrain, xValid, yValid) = trainTestSplit(xTrainValidate, yTrainValidate, splitRatio._1 / (splitRatio._1 + splitRatio._2), shuffledCol)
        (xTrain, yTrain, xValid, yValid, xTest, yTest)
    }

    def trainTestSplit(features: Seq[Seq[Any]], labels: Seq[Int] = Seq(), splitRatio: Double = 0.7, shuffledCol: Int = -1):
    (Seq[Seq[Any]], Seq[Int], Seq[Seq[Any]], Seq[Int]) = {
        val useData = if (labels.isEmpty) features else features.zip(labels).map(v => v._1 :+ v._2)
        def splitData(data: Seq[Seq[Any]]): (Seq[Seq[Any]], Seq[Int], Seq[Seq[Any]], Seq[Int]) = {
            if (shuffledCol == -1) {
                val shuffledData = Random.shuffle(data.toSeq).toSeq
                (shuffledData.slice(0, (splitRatio * data.length).round.toInt).map(_.init), shuffledData.slice(0, (splitRatio * data.length).round.toInt).map(_.last.asInstanceOf[Int]),
                    shuffledData.slice((splitRatio * data.length).round.toInt, data.length).map(_.init), shuffledData.slice((splitRatio * data.length).round.toInt, data.length).map(_.last.asInstanceOf[Int]))
            } else {
                val mapData = data.groupBy(v => v(shuffledCol))
                val shuffledData = Random.shuffle(mapData).map(_._2).toSeq
                (shuffledData.slice(0, (splitRatio * shuffledData.length).round.toInt).flatten.map(_.init), shuffledData.slice(0, (splitRatio * shuffledData.length).round.toInt).flatten.map(_.last.asInstanceOf[Int]),
                    shuffledData.slice((splitRatio * shuffledData.length).round.toInt, shuffledData.length).flatten.map(_.init), shuffledData.slice((splitRatio * shuffledData.length).round.toInt, shuffledData.length).flatten.map(_.last.asInstanceOf[Int]))
            }
        }
        // 注意： 先满足了正负样本的均衡再满足相同归属的样本同分到训练/测试,所以如果一个归属有正有负，他是有可能被分到训练集+测试集中的，
        // 但是就算分到两边，两边的label也是不一样的，所以不存在过拟合的风险，只是可能会让模型效果差一些
        val positiveSplitData = splitData(useData.filter(_.last.toString == "1"))
        val negativeSplitData = splitData(useData.filter(_.last.toString == "0"))
        (positiveSplitData._1 ++ negativeSplitData._1, positiveSplitData._2 ++ negativeSplitData._2, positiveSplitData._3 ++ negativeSplitData._3, positiveSplitData._4 ++ negativeSplitData._4)
    }

    def kFoldSplit(features: Seq[Seq[Any]], labels: Seq[Int] = Seq(), k: Int = 10):
    (Seq[Seq[Any]], Seq[Int], Seq[Seq[Any]], Seq[Int]) = {
        trainTestSplit(features, labels, 1 - 1.0 / k)
    }

    def bootStrapping(features: Seq[Seq[Any]], labels: Seq[Int] = Seq()):
    (Seq[Seq[Any]], Seq[Int]) = {
        var train = Seq[Seq[Any]]()
        val useData = if (labels.isEmpty) features else features.zip(labels).map(v => v._1 :+ v._2)
        while(train.length < features.length) {
            train :+= useData((new util.Random).nextInt(features.length))
        }
        (train.map(_.init), train.map(_.last.asInstanceOf[Int]))
    }
}
