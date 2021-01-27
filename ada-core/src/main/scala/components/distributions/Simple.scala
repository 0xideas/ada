package ada.core.components.distributions

import breeze.stats.distributions.{Beta, Bernoulli}
import io.circe.Json

import ada._


class BetaDistribution (private var alpha: Double, private var beta: Double, learningRate: Double = 1.0)
    extends SimpleDistribution{
    private var betaDistribution = Beta(alpha, beta)
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

    def export: Json = Json.fromFields(Map(
        "alpha" -> Json.fromDouble(alpha).get,
        "beta" -> Json.fromDouble(beta).get
    ))
}



class MeanDouble extends SimpleDistribution {
    private var i = 1.0
    private var value = 0.0
    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {
        val oldValue = value
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            value = value*(1.0-1.0/i) + reward.value * (1.0/i)
            i+=1.0
        }
    }
}

class Exp3Reward(private var value: Double, gamma: Double, k: Int) extends SimpleDistribution{
    def draw: Double = value

    def update(reward: Reward): Unit = {
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            value = value * math.exp(gamma* reward.value/(k))
        }
    }

    def export: Json = Json.fromFields(Map(
        "value" -> Json.fromDouble(value).get,
        "gamma" -> Json.fromDouble(gamma).get,
        "k" -> Json.fromInt(k)
    ))
}



//TEST

class ExpDouble(private var value: Double) extends SimpleDistribution {
    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {if(!(reward.value.isInfinite || reward.value.isNaN() )){value = reward.value}; ()}
}

object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble( d)
}