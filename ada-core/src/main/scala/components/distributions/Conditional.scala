package ada.components.distributions

import smile.regression.{OnlineRegression, LinearModel}
import smile.regression.lm
import smile.data.formula.Formula
import smile.data.DataFrame

import ada.components.learners.{BayesianSampleLinearRegression, BayesianMeanLinearRegression}
import ada._

//this will be removed
abstract class SmileModelConditionalDistribution[Context <: Array[Double], SmileModel <: OnlineRegression[Context]](val model: SmileModel)
    extends ConditionalDistribution[Context]{
        def draw(context: Context): Double = model.predict(context)
        def update(context: Context, reward: Reward): Unit = {
            if(!(reward.value.isInfinite || reward.value.isNaN() )){
                model.update(context, reward.value)
            }
        }   
}

class PointRegressionDistribution(
    formula: Formula,
    data: DataFrame,
    method: String = "qr",
    stderr: Boolean = true,
    recursive: Boolean = true)
    extends SmileModelConditionalDistribution[Array[Double], LinearModel](
        lm(formula, data, method, stderr, recursive)
    )


class BayesianSampleRegressionDistribution(
    protected[distributions] var nfeatures: Int,
    protected[distributions] var alpha: Double = 0.3,
    protected[distributions] var beta: Double = 1.0)
    extends SmileModelConditionalDistribution[Array[Double], BayesianSampleLinearRegression](
        new BayesianSampleLinearRegression(nfeatures, alpha, beta)
    )
    with ConditionalDistribution[Array[Double]]{
        def setBeta(increment: Double = 0.0, factor: Double = 1.0, max: Double = 5000.0): Unit = {
            model.changeBeta(increment, factor, max)
        }
        def getBeta(): Double = model.getBeta

    }


class BayesianMeanRegressionDistribution(
    val nfeatures: Int,
    val alpha: Double = 0.3,
    beta: Double = 1.0)
    extends SmileModelConditionalDistribution[Array[Double], BayesianMeanLinearRegression](
        new BayesianMeanLinearRegression(nfeatures, alpha, beta)
    )
    with ConditionalDistribution[Array[Double]]{
        def setBeta(increment: Double = 0.0, factor: Double = 1.0, max: Double = 5000.0): Unit = {
            model.changeBeta(increment, factor, max)
        }
        def getBeta(): Double = model.getBeta
    }
