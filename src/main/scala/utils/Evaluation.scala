package utils

/**
  * error
  * accuracy
  * tp fp fn tn
  * precision recall
  * f-sore
  * ROC
  */
object Evaluation {

    def accuracy(labels: Seq[Int], predictions: Seq[Int]): Double = {
        assert(labels.length == predictions.length)
        MathUtils.round8(1.0 * labels.zip(predictions).count(v => v._1 == v._2) / labels.length)
    }

    def error(labels: Seq[Int], predictions: Seq[Int]): Double = {
        assert(labels.length == predictions.length)
        MathUtils.round8(1.0 - accuracy(labels, predictions))
    }

    def truePositive(labels: Seq[Int], predictions: Seq[Int]): Int = {
        assert(labels.length == predictions.length)
        labels.zip(predictions).count(v => v._1 == 1 && v._2 == 1)
    }

    def falsePositive(labels: Seq[Int], predictions: Seq[Int], cost: Int = 1): Int = {
        assert(labels.length == predictions.length)
        labels.zip(predictions).count(v => v._1 == 0 && v._2 == 1) * cost
    }

    def falseNegative(labels: Seq[Int], predictions: Seq[Int], cost: Int = 1): Int = {
        assert(labels.length == predictions.length)
        labels.zip(predictions).count(v => v._1 == 1 && v._2 == 0) * cost
    }

    def trueNegative(labels: Seq[Int], predictions: Seq[Int]): Int = {
        assert(labels.length == predictions.length)
        labels.zip(predictions).count(v => v._1 == 0 && v._2 == 0)
    }

    def precision(labels: Seq[Int], predictions: Seq[Int], cost: Int = 1): Double = {
        if (predictions.sum == 0) return 0.0
        MathUtils.round8(1.0 * truePositive(labels, predictions) / (truePositive(labels, predictions) + falsePositive(labels, predictions, cost)))
    }

    def recall(labels: Seq[Int], predictions: Seq[Int], cost: Int = 1): Double = {
        if (labels.sum == 0) return 1.0
        MathUtils.round8(1.0 * truePositive(labels, predictions) / (truePositive(labels, predictions) + falseNegative(labels, predictions, cost)))
    }

    /**
      *
      * 根据阈值计算PR曲线(每个阈值能对应出来一对(precision，recall))
      * 粗暴算法：把所有的threshold都找到，根据threshold将score转化为binary，然后一个一个计算(precision, recall)
      * 优雅算法：其实只需要按照score将labels降序排序后在用cumsum求和就可以了，因为计算只要看"当前阈值"包含了多少个1，就知道了precision，知道一共有多少个1，再看当前的就知道了recall
      */
    def precisionRecallCurve(labels: Seq[Int], predictionsPossibility: Seq[Double], cost: (Int, Int) = (1, 1), degree: Int = 4): Seq[(Double, Double, Double)] = {
        assert(labels.length == predictionsPossibility.length)
        val labelsPredictions = labels.zip(predictionsPossibility.map(("%." + degree + "G").format(_).toDouble)).sortBy(_._2).reverse
        val indexes = labelsPredictions.map(_._2).zip(labelsPredictions.map(_._2).tail).map {var s = -1; v => { s += 1; if (v._1 != v._2) s else -1 }}.filter(_ != -1) ++ Array(labelsPredictions.length - 1)
        val tps = labelsPredictions.map(_._1).map {var s = 0; v => {s += v; s}}
        val fps = tps.map {var s = 0; v => {s += 1; s - v}}
        val precision = for (i <- tps.indices) yield { tps(i).toDouble / (tps(i) + fps(i))}
        val recall = tps.map(_.toDouble / tps.last)
        (indexes.map(recall).map(v => if (v.isNaN) 1 else MathUtils.round8(v)), indexes.map(precision).map(v => if (v.isNaN) 0 else MathUtils.round8(v)), indexes.map(labelsPredictions).map(_._2)).zipped.toSeq
    }

    def fScore_(r: Double, p: Double, beta: Double = 1.0): Double = {
        MathUtils.round8(p * r * (1 + math.pow(beta, 2)) / (math.pow(beta, 2) * p + r))
    }

    /**
      * f-socre是调和平均 (1 / p + 1 / r)的变种，相比算术平均和几何平均更注重较小值
      */
    def fScore(labels: Seq[Int], predictions: Seq[Int], beta: Double = 1.0): Double = {
        if (beta <= 0) throw new Exception("The beta should larger than 0")
        val p = precision(labels, predictions)
        val r = recall(labels, predictions)
        fScore_(r, p, beta)
    }

    /**
      * 所有准确率的平均数和所有召回率的平均数作为总体的precision recall
      */
    def macroPRF(labels: Seq[Seq[Int]], predictions: Seq[Seq[Int]], beta: Double = 1.0, cost: (Int, Int) = (1, 1)): (Double, Double, Double) = {
        assert(labels.length == predictions.length)
        for (i <- labels.indices) assert(labels(i).length == predictions(i).length)

        if (labels.length != predictions.length) throw new Exception("The length of labels and the length of predictions is not the same!")
        var macro_p : Double = 0.0
        var macro_r : Double = 0.0
        for (i <- labels.indices) {
            macro_p += precision(labels(i), predictions(i), cost._1)
            macro_r += recall(labels(i), predictions(i), cost._2)
        }
        macro_p *= 1.0 / labels.length
        macro_r *= 1.0 / labels.length
        (MathUtils.round8(macro_r), MathUtils.round8(macro_p), fScore_(macro_p, macro_r, beta))
    }

    /**
      * 所有tp, fp, fn平均数去计算总体的precision recall
      */
    def microPRF(labels: Seq[Seq[Int]], predictions: Seq[Seq[Int]], beta: Double = 1.0, cost: (Int, Int) = (1, 1)): (Double, Double, Double) = {
        assert(labels.length == predictions.length)
        for (i <- labels.indices) assert(labels(i).length == predictions(i).length)

        if (labels.length != predictions.length) throw new Exception("The length of labels and the length of predictions is not the same!")
        var tp : Double = 0.0
        var fp : Double = 0.0
        var fn : Double = 0.0
        for (i <- labels.indices) {
            tp += truePositive(labels(i), predictions(i))
            fp += falsePositive(labels(i), predictions(i), cost._1)
            fn += falseNegative(labels(i), predictions(i), cost._2)
        }
        tp *= 1.0 / labels.length
        fp *= 1.0 / labels.length
        fn *= 1.0 / labels.length
        val micro_p = tp / (tp + fp)
        val micro_r = tp / (tp + fn)
        (MathUtils.round8(micro_r), MathUtils.round8(micro_p), MathUtils.round8(fScore_(micro_p, micro_r, beta)))
    }

    /**
      * 如果predictionsPossibility全是0， fpr应该是0（代码中是1），不过这些也没啥意义
      */
    def ROC(labels: Seq[Int], predictionsPossibility: Seq[Double], degree: Int = 4): Seq[(Double, Double, Double)] = {
        assert(labels.length == predictionsPossibility.length)

        val labelsPredictions = labels.zip(predictionsPossibility.map(("%." + degree + "G").format(_).toDouble)).sortBy(_._2).reverse
        val indexes = labelsPredictions.map(_._2).zip(labelsPredictions.map(_._2).tail).map {var s = -1; v => { s += 1; if (v._1 != v._2) s else -1 }}.filter(_ != -1) ++ Array(labelsPredictions.length - 1)
        val tps = labelsPredictions.map(_._1).map {var s: Double = 0.0; v => {s += v; s}}
        val fps = tps.map {var s: Double = 0.0; v => {s += 1; s - v}}
        val tpr = tps.map(_ / tps.last)
        val fpr = fps.map(_ / fps.last)
        (indexes.map(fpr).map(v => if (v.isNaN) 1 else MathUtils.round8(v)), indexes.map(tpr).map(v => if (v.isNaN) 0 else MathUtils.round8(v)), indexes.map(labelsPredictions).map(_._2)).zipped.toSeq
    }

    /**
      * AUC of ROC / PR
      */
    def calculateCurveArea(coordinate: Seq[(Double, Double)]): Double = {
        val sortedCoordinate = (coordinate :+ (1.0, 1.0)).distinct.sortBy(_._1)
        var area: Double = 0.0
        var preX: Double = 0.0
        var preY: Double = 0.0
        sortedCoordinate.foreach { v =>
            if (v._1 != preX) {area += (v._1 - preX) * (v._2 + preY) / 2 }
            preX = v._1
            preY = v._2
        }
        MathUtils.round8(area)
    }

    def calculateRegressionError(predictions: Seq[Double], labels: Seq[Double], method: String = "mse"): Double = {
        method match {
            case "mse" => Evaluation.meanSquareError(predictions, labels)
            case "rmse" => Evaluation.rootMeanSquareError(predictions, labels)
            case "rs" => Evaluation.rSquared(predictions, labels)
            case "mae" => Evaluation.meanAbsoluteError(predictions, labels)
            case "mape" => Evaluation.meanAbsolutePercentError(predictions, labels)
            case _ =>  System.err.println("The method is unknown or not implemented."); 0.0
        }
    }

    def meanSquareError(y: Seq[Double], targets: Seq[Double]): Double = {
        assert(y.length == targets.length)
        MathUtils.round8(1.0 / y.length * y.zip(targets).foldLeft(0.0)((v1, v2) => v1 + math.pow(v2._1 - v2._2, 2)))
    }

    def rootMeanSquareError(y: Seq[Double], targets: Seq[Double]): Double = {
        MathUtils.round8(math.sqrt(meanSquareError(y, targets)))
    }

    /**
      * r-squared应该是 1 - sum(pow(yi - targetMean, 2)) / sum(pow(targeti - targetMean, 2))
      * 但是看了下 python中 sklearn.metrics.r2_score的实现是 1 - sum(pow(yi - targeti, 2)) / sum(pow(targeti - targetMean, 2))
      * 这里采用python中的写法
      */
    def rSquared(y: Seq[Double], targets: Seq[Double]): Double = {
        assert(y.length == targets.length)
        val mean = targets.sum / targets.length
        if (targets.count(_ == mean) == targets.length) return 0.0
        MathUtils.round8(1 - y.zip(targets).foldLeft(0.0)((v1, v2) => v1 + math.pow(v2._1 - v2._2, 2)) /
            targets.foldLeft(0.0)((v1, v2) => v1 + math.pow(v2 - mean, 2)))
    }

    def meanAbsoluteError(y: Seq[Double], targets: Seq[Double]): Double = {
        assert(y.length == targets.length)
        MathUtils.round8(1.0 / y.length * y.zip(targets).foldLeft(0.0)((v1, v2) => v1 + math.abs(v2._1 - v2._2)))
    }

    def meanAbsolutePercentError(y: Seq[Double], targets: Seq[Double]): Double = {
        assert(y.length == targets.length)
        MathUtils.round8(1.0 / y.length * y.zip(targets).foldLeft(0.0)((v1, v2) => v1 + math.abs(v2._1 - v2._2) / v2._2))
    }

}
