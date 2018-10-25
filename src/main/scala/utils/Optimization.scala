package utils

import breeze.linalg.{DenseMatrix, trace}

object Optimization {

    // srqt(trace(A.T * A))
    def FrobeniusNorm(A: DenseMatrix[Double]): Double = {
        math.sqrt(trace(A.t * A))
    }

    /**
      * 共轭梯度下降法
      * @return c: 系数矩阵
      */
    def conjugateGradient(x: Seq[Seq[Double]], y: Seq[Double], maxIteration: Int = 10000, tol: Double = 1e-6): Seq[Double] = {
        val A = DenseMatrix(x: _*)
        val b = DenseMatrix(y: _*)
        // val c: Seq[Double] = Seq.fill(x.head.length)(1)
        val c = DenseMatrix((1 to x.head.length).map(_ => 1.0): _*)

        var r = b - A * c
        var p = r
        var iteration = 0

        while (iteration < maxIteration && FrobeniusNorm(A * c - b) / FrobeniusNorm(b) >= tol) {
            iteration += 1
            val rRaw = r
            val alpha = (r.t * r) / (p.t * A * p)
            c += (alpha * p.t).t
            r = b - A * c
            val beta = math.pow(FrobeniusNorm(r), 2) / math.pow(FrobeniusNorm(rRaw), 2)
            p = r + beta * p
        }
        c.toArray
    }
}
