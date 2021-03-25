package ada.components.distributions
import ada.interface.{LTree, LBranch, LLeaf, LTwig}

import ada.components.learners.{BayesianSampleLinearRegression, BayesianMeanLinearRegression}
import ada._

trait OnlineRegression[Independent]{
    def predict(data: Independent): Double
    def update(data: Independent, rewardValue: LTree[Double])
}

//this will be removed
abstract class ConditionalDistributionI[Context <: Array[Double], SmileModel <: OnlineRegression[Context]](val model: SmileModel)
    extends ConditionalDistribution[Context]{
        def draw(context: Context): Double = model.predict(context)
        def update(context: Context, reward: Reward): Unit = {
            if(!(reward.value.isInfinite || reward.value.isNaN())){
                model.update(context, new LLeaf(reward.value))
            }
        }   
}




class BayesianSampleRegressionDistribution(
    protected[distributions] var nfeatures: Int,
    protected[distributions] var alpha: Double = 0.3,
    protected[distributions] var beta: Double = 1.0)
    extends ConditionalDistributionI[Array[Double], BayesianSampleLinearRegression](
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
    extends ConditionalDistributionI[Array[Double], BayesianMeanLinearRegression](
        new BayesianMeanLinearRegression(nfeatures, alpha, beta)
    )
    with ConditionalDistribution[Array[Double]]{
        def setBeta(increment: Double = 0.0, factor: Double = 1.0, max: Double = 5000.0): Unit = {
            model.changeBeta(increment, factor, max)
        }
        def getBeta(): Double = model.getBeta
    }



/*
class PointRegressionDistribution(
    formula: Formula,
    data: DataFrame,
    method: String = "qr",
    stderr: Boolean = true,
    recursive: Boolean = true)
    extends ConditionalDistributionI[Array[Double], LinearModel](
        lm(formula, data, method, stderr, recursive)
    )*/