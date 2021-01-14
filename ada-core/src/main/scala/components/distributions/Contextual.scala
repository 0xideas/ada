package ada.core.components.distributions

import smile.regression.{OnlineRegression, LinearModel}
import smile.regression.lm
import smile.data.formula.Formula
import smile.data.DataFrame
import io.circe.Json

import breeze.linalg._

import ada.core.components.contextmodels.BayesianSampleLinearRegression
import ada._

class SmileModelContextDistribution[Context <: Array[Double], SmileModel <: OnlineRegression[Context]](val model: SmileModel)
    extends ContextualDistribution[Context]{
        def draw(context: Context): Double = model.predict(context)
        def update(context: Context, reward: Reward): Unit = model.update(context, reward)
        def export: Json = Json.fromString(model.toString())
}

class PointRegressionContext(
    formula: Formula,
    data: DataFrame,
    method: String = "qr",
    stderr: Boolean = true,
    recursive: Boolean = true)
    extends SmileModelContextDistribution[Array[Double], LinearModel](
        lm(formula, data, method, stderr, recursive)
    )

class BayesianSampleRegressionContext(
    val nfeatures: Int,
    val alpha: Double = 0.3,
    beta: Double = 1.0)
    extends SmileModelContextDistribution[Array[Double], BayesianSampleLinearRegression](
        new BayesianSampleLinearRegression(nfeatures, alpha, beta)
    ){
        override def export: Json = model.export
        def changeBeta(increment: Double = 0.0, factor: Double = 1.0, max: Double = 5000.0): Unit = {
            model.changeBeta(increment, factor, max)
        }
        def beta(): Double = model.beta

        def setParameters(mean: Array[Double], covInv: Array[Double]): Unit = {
            model.set(DenseVector(mean), DenseMatrix(covInv).reshape(nfeatures, nfeatures))
        } 

    }

