package epsilon.distributions

import breeze.stats.distributions.{Beta, Bernoulli}


trait Distribution[Reward <: Double]{
    def draw: Double
    def update(reward: Reward): Unit
}

class BetaDistribution[Reward <: Double](private var alpha: Double, private var beta: Double)
    extends Distribution[Reward]{
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