package utils

import utils.NormalizationType.NormalizationType

object NormalizationType extends Enumeration {
    type NormalizationType = Value
    val None: NormalizationType = Value("None")
    val MinMax: NormalizationType = Value("MinMax")
    val Max: NormalizationType = Value("Max")
    val ZScore: NormalizationType = Value("ZScore")
}

object NormalizationMethod {

    def normalizeByRow(x: Seq[Seq[Double]], normMethod: NormalizationType): Seq[Seq[Double]] = {
        x.map(normalize(_, normMethod))
    }

    def normalizeByColumn(x: Seq[Seq[Double]], normMethod: NormalizationType): Seq[Seq[Double]] = {
        x.transpose.map(normalize(_, normMethod)).transpose
    }

    def normalize(x: Seq[Double], method: NormalizationType): Seq[Double] = {
        if (x.isEmpty) return x
        method match {
            case NormalizationType.None => x
            case NormalizationType.MinMax => minMax(x)
            case NormalizationType.Max => max(x)
            case NormalizationType.ZScore => zScore(x)
            case _ => throw new NotImplementedError("Not implemented this NormalizationType yet.")
        }
    }

    def minMax(x: Seq[Double]): Seq[Double] = {
        if (x.toSet.size <= 1) return List.fill(x.length)(1)
        val maxx = x.max
        val minn = x.min
        x.map(v => (v - minn) / (maxx - minn))
    }

    def max(x: Seq[Double]): Seq[Double] = {
        if (x.toSet.size <= 1) return List.fill(x.length)(1)
        val maxx = x.max
        x.map(_ / maxx)
    }

    def zScore(x: Seq[Double]): Seq[Double] = {
        if (x.toSet.size <= 1) return List.fill(x.length)(1)
        val mean = MathUtils.mean(x)
        val std = MathUtils.std(x)
        x.map(v => (v - mean) / std)
    }

}
