package epsilon.core.components.distributions

import epsilon._
import breeze.stats.distributions.{Beta, Bernoulli}

import smile.regression.lm
import smile.data.formula.Formula
import smile.data.DataFrame
import smile.regression.{OnlineRegression, LinearModel}

sealed trait Distribution

trait SimpleDistribution extends Distribution{
    def draw: Double
    def update(reward: Reward): Unit
}

trait ContextualDistribution[Context] extends Distribution{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit
}

class BetaDistribution (private var alpha: Double, private var beta: Double)
    extends SimpleDistribution{
    private var betaDistribution = Beta(alpha, beta)
    override def toString: String = {
        f"alpha: $alpha beta: $beta"
    }

    def draw = betaDistribution.draw()

    def update(reward:Reward):Unit = {
        val rewardNormed = math.max(math.min(reward, 1), 0)
        alpha = alpha + rewardNormed
        beta = beta + (1.0-rewardNormed)
        betaDistribution = Beta(alpha, beta)
    }
}



class PointSmileModelContext[Context <: Array[Double], SmileModel <: OnlineRegression[Context]](val model: SmileModel)
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
    extends PointSmileModelContext[Array[Double], LinearModel](lm(formula, data, method, stderr, recursive))

    /*
class PointRegressionContext[Context <: Array[Double]](
    formula: Formula,
    data: DataFrame,
    method: String = "qr",
    stderr: Boolean = true,
    recursive: Boolean = true)
    extends ContextualDistribution[Context]{
    
    val model = lm(formula, data, method, stderr, recursive)

    def draw(context: Context): Double = {
        model.predict(context)
    }
    def update(context:Context, reward: Reward): Unit = {
        model.update(context, reward)
    }
}*/