package ada.core.components.distributions

import smile.regression.{OnlineRegression, LinearModel}
import smile.regression.lm
import smile.data.formula.Formula
import smile.data.DataFrame
import io.circe.Json

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
    nfeatures: Int,
    alpha: Double = 0.3,
    beta: Double = 1.0,
    betaDecay: Double = 1.0)
    extends SmileModelContextDistribution[Array[Double], BayesianSampleLinearRegression](
        new BayesianSampleLinearRegression(nfeatures, alpha, beta, betaDecay)
    ){
        override def export: Json = model.export
    }
