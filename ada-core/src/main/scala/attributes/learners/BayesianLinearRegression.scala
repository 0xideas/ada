package ada.components.learners

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}
import smile.regression.{OnlineRegression, LinearModel}

import scala.language.implicitConversions
import ada.interface.Settable
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

abstract class BayesianLinearRegressionAbstract(private var nfeatures: Int, private var alpha: Double, private var beta: Double)
    extends OnlineRegression[Array[Double]]{
    private var mean = DenseVector.zeros[Double](nfeatures)
    private var covInv = DenseMatrix.eye[Double](nfeatures).map(_/alpha)
    private var cov = DenseMatrix.zeros[Double](nfeatures, nfeatures)
    private var w_cov = DenseMatrix.zeros[Double](nfeatures, nfeatures)

    implicit def toVector(array: Array[Double]): DenseVector[Double] = DenseVector(array:_*)

    private case class Update(nfeatures: Int, alpha: Double, beta: Double, mean: Array[Double], covInv: Array[Double])
    private implicit val updateDecoder: Decoder[Update] = deriveDecoder[Update]
    private implicit val updateEncoder: Encoder[Update] = deriveEncoder[Update]

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
    def setParameters(parameters: Json): Unit = {
        val pars = parameters.as[Update]
        pars match {
            case Right(Update(nfeaturesV, alphaV, betaV, meanV, covInvV)) => {
                nfeatures = nfeaturesV
                alpha = alphaV
                beta = betaV
                setMean(DenseVector(meanV))
                setCovInv(DenseMatrix(covInvV).reshape(nfeatures, nfeatures))

            }
            case Left(decodingFailure) => println(decodingFailure)
        }
    }

    def export: Json = Update(nfeatures, alpha, beta, mean.toArray, covInv.toArray).asJson

    def changeBeta(increment: Double = 0.0, factor: Double = 1.0, max: Double = 5000.0): Unit = {
        val newBeta = math.min((beta+increment)*factor, max)
        println(newBeta)
        beta = newBeta
    }
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




