package epsilon.core.components.distributions

import smile.regression.{OnlineRegression, LinearModel}
import smile.regression.lm
import smile.data.formula.Formula
import smile.data.DataFrame

import epsilon.core.components.contextmodels.BayesianLinearRegressionSample
import epsilon._

class SmileModelContextDistribution[Context <: Array[Double], SmileModel <: OnlineRegression[Context]](val model: SmileModel)
    extends ContextualDistribution[Context]{
        def draw(context: Context): Double = model.predict(context)
        def update(context: Context, reward: Reward): Unit = model.update(context, reward)
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

class BayesianRegressionSampleContext(
    nfeatures: Int,
    alpha: Double = 0.3,
    beta: Double = 1.0)
    extends SmileModelContextDistribution[Array[Double], BayesianLinearRegressionSample](
        new BayesianLinearRegressionSample(nfeatures, alpha, beta)
    )
