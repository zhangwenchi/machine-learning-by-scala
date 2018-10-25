package models

/**
  * least absolute shrinkage and selection operator
  * 借用l1正则化可得稀疏解
  * The objective function in the L1-regularized least squares is convex but not differentiable 凸但是不可微分
  */
class LassoRegression {

    def fit(x: Seq[Seq[Double]], y: Seq[Double], lambda: Double = 1.0, tol: Double = 1e-6, maxIter: Int = 1000): Unit = {
        if (lambda <= 0.0) throw new IllegalArgumentException("Lambda of LASSO should be larger than 0.")
        if (tol <= 0) throw new IllegalArgumentException("Tolerance of LASSO should be larger than 0.")
        if (maxIter <= 0) throw new IllegalArgumentException("maxIteration of LASSO should be larger than 0.")



    }
}
