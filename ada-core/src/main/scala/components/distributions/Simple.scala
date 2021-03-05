package ada.components.distributions

import breeze.stats.distributions.{Beta, Bernoulli}

import ada._


class BetaDistribution (protected[distributions] var alpha: Double,
                        protected[distributions] var beta: Double,
                        protected[distributions] var learningRate: Double = 1.0)
    extends SimpleDistribution{
    protected[distributions] var betaDistribution = Beta(alpha, beta)
    override def toString: String = {
        f"alpha: $alpha beta: $beta"
    }

    def draw = betaDistribution.draw()

    def updateBounded(reward: Reward): Unit = {
        val rewardNormed = math.max(math.min(reward.value, 1), 0)
        alpha = alpha + rewardNormed
        beta = beta + (1.0-rewardNormed)
        betaDistribution = Beta(alpha, beta)
    }

    def update(reward: Reward): Unit = {
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            alpha = alpha  + math.max(0, reward.value)*learningRate
            beta = beta + math.max(0, 1 - reward.value)*learningRate
            betaDistribution = Beta(alpha, beta)
        }
    }

}



class MeanDouble extends SimpleDistribution{
    protected[distributions] var i = 1.0
    protected[distributions] var value = 0.0

    def draw: Double = value
    def update(reward: Reward): Unit = {
        val oldValue = value
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            value = value*(1.0-1.0/i) + reward.value * (1.0/i)
            i+=1.0
        }
    }

}



class Exp3Reward(protected[distributions] var value: Double,
                protected[distributions] var gamma: Double,
                protected[distributions] var k: Int) extends SimpleDistribution{

    def draw: Double = value

    def update(reward: Reward): Unit = {
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            value = value * math.exp(gamma* reward.value/(k))
        }
    }
}



//TEST

class ExpDouble(protected[distributions] var value: Double) extends SimpleDistribution {
    def draw: Double = value
    def update(reward: Reward): Unit = {if(!(reward.value.isInfinite || reward.value.isNaN() )){value = reward.value}; ()}

}

object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble( d)
}