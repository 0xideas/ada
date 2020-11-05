package epsilon.core.components.distributions

import breeze.stats.distributions.{Beta, Bernoulli}


trait Distribution

trait SimpleDistribution[Reward <: Double] extends Distribution{
    def draw: Double
    def update(reward: Reward): Unit
}

trait ContextualDistribution[Context, Reward <: Double] extends Distribution{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit
}

class BetaDistribution[Reward <: Double](private var alpha: Double, private var beta: Double)
    extends SimpleDistribution[Reward]{
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