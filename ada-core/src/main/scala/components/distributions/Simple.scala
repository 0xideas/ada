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
        val rewardNormed = math.max(math.min(reward, 1), 0)
        alpha = alpha + rewardNormed
        beta = beta + (1.0-rewardNormed)
        betaDistribution = Beta(alpha, beta)
    }

    def update(reward: Reward): Unit = {
        if(!(reward.isInfinite || reward.isNaN() )){
            alpha = alpha  + math.max(0, reward)*learningRate
            beta = beta + math.max(0, 1 - reward)*learningRate
            betaDistribution = Beta(alpha, beta)
        }
    }

    def export: Json = Json.fromFields(Map(
        "alpha" -> Json.fromDouble(alpha).get,
        "beta" -> Json.fromDouble(beta).get
    ))
}


class ExpDouble(private var value: Double) extends SimpleDistribution {
    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {if(!(reward.isInfinite || reward.isNaN() )){value = reward}; ()}
}
object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble(d) 
}

class MeanDouble(private var value: Double) extends SimpleDistribution {
    private var i = 1.0
    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {
        val oldValue = value
        if(!(reward.isInfinite || reward.isNaN() )){
            value = value*(1.0-1.0/i) + reward * (1.0/i)
            i+=1.0
        }
        //println(f"${oldValue} - ${reward} - ${value}")
    }
}

class Exp3Reward(private var value: Double, gamma: Double, k: Int) extends SimpleDistribution{
    def draw: Double = value

    def update(reward: Reward): Unit = {
        if(!(reward.isInfinite || reward.isNaN() )){
            value = value * math.exp(gamma* reward/(k))
        }
    }

    def export: Json = Json.fromFields(Map(
        "value" -> Json.fromDouble(value).get,
        "gamma" -> Json.fromDouble(gamma).get,
        "k" -> Json.fromInt(k)
    ))
}
