package epsilon.core.components.linear

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}

class BayesianLinearRegression(val nfeatures: Int, val alpha: Double, val beta: Double){
    private var mean = DenseVector.zeros[Double](nfeatures)
    private var covInv = DenseMatrix.eye[Double](nfeatures).map(_/alpha)
    private var cov = DenseMatrix.zeros[Double](nfeatures, nfeatures)

    implicit def toVector(array: Array[Double]): DenseVector[Double] = DenseVector(array:_*)

    def learn(x: Array[Double], y: Double): Unit = {
        val xvec = toVector(x)
        val outer = (xvec * xvec.t)
        covInv = covInv + outer.map(_ * beta)
        cov = inv(covInv)
        mean = cov * ((covInv * mean) + (xvec.map(_ * beta * y)))
    }

    def predict(x: Array[Double]): Gaussian = {
        val xvec = toVector(x)
        val y_pred_mean = xvec.t * mean

        val w_cov = inv(covInv)
        val y_pred_var = (1/ beta) + (xvec.t * w_cov * xvec)
        new Gaussian(y_pred_mean, y_pred_var)
    }

    def weights_dist:MultivariateGaussian = {
        MultivariateGaussian(mean, cov)
    }
}






