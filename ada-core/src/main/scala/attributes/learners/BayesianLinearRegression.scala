package ada.components.learners

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}
import smile.regression.{OnlineRegression, LinearModel}

import scala.language.implicitConversions
import ada.components.distributions.ConditionalDistribution
import ada.`package`.Reward

abstract class BayesianLinearRegressionAbstract(protected[components] var nfeatures: Int, protected[components] var alpha: Double, protected[components] var beta: Double)
    extends OnlineRegression[Array[Double]]
    with ConditionalDistribution[Array[Double]]{
    protected[components] var mean = DenseVector.zeros[Double](nfeatures)
    protected[components] var covInv = DenseMatrix.eye[Double](nfeatures).map(_/alpha)
    protected[components] var cov = DenseMatrix.zeros[Double](nfeatures, nfeatures)
    protected[components] var w_cov = DenseMatrix.zeros[Double](nfeatures, nfeatures)

    implicit def toVector(array: Array[Double]): DenseVector[Double] = DenseVector(array:_*)

    def getBeta() = beta

    def update(x: Array[Double], y: Double): Unit = {
        if(!(y.isInfinite || y.isNaN() )){
            val xvec = toVector(x)
            val outer = (xvec * xvec.t)
            val covInvT = covInv + outer.map(_ * beta)
            cov = inv(covInvT)
            mean = cov * ((covInv * mean) + (xvec.map(_ * beta * y)))
            covInv = covInvT

            w_cov = cov
        }
        //w_cov = inv(covInv)
    }

    def predictProb(x: Array[Double]): Gaussian = {
        val xvec = toVector(x)
        val y_pred_mean = xvec.t * mean

        //w_cov = inv(covInv)
        val y_pred_var = (1/ beta) + (xvec.t * w_cov * xvec)
        new Gaussian(y_pred_mean, y_pred_var)
    }

    def weights: MultivariateGaussian = {
        MultivariateGaussian(mean, cov)
    }

    def setMean(mean: DenseVector[Double]): Unit = {
        require(mean.length == this.mean.length)
        this.mean = mean
    }

    def setCovInv(covInv: DenseMatrix[Double]): Unit = {
        require(covInv.size == this.covInv.size)
        this.covInv = covInv
    }

    def set(mean: DenseVector[Double], covInv: DenseMatrix[Double]): Unit = {
        setMean(mean)
        setCovInv(covInv)
    }

    def changeBeta(increment: Double = 0.0, factor: Double = 1.0, max: Double = 5000.0): Unit = {
        val newBeta = math.min((beta+increment)*factor, max)
        println(newBeta)
        beta = newBeta
    }
    def update(context: Array[Double], reward: Reward): Unit = update(context, reward)
    def predict(x: Array[Double]): Double 
    def draw(context: Array[Double]): Double = predict(context)

}


class BayesianSampleLinearRegression(nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta){
    def predict(x: Array[Double]): Double = predictProb(x).sample
}

//basically identical to point estimate linear regression - not used at the moment
class BayesianMeanLinearRegression(nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta){
    def predict(x: Array[Double]): Double = predictProb(x).mean

}




