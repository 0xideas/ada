package ada.core.components.distributions

import breeze.stats.distributions.{Beta, Bernoulli}
import io.circe.Json

import ada._


class BetaDistribution (private var alpha: Double, private var beta: Double)
    extends SimpleDistribution{
    private var betaDistribution = Beta(alpha, beta)
    override def toString: String = {
        f"alpha: $alpha beta: $beta"
    }

    def draw = betaDistribution.draw()

    def updateBounded(reward:Reward):Unit = {
        val rewardNormed = math.max(math.min(reward, 1), 0)
        alpha = alpha + rewardNormed
        beta = beta + (1.0-rewardNormed)
        betaDistribution = Beta(alpha, beta)
    }
    def update(reward:Reward):Unit = {
        alpha = alpha + math.max(0, reward)
        beta = beta - math.min(0, reward)
        betaDistribution = Beta(alpha, beta)
    }
    def updateRecency(reward:Reward, recencyBias:Double):Unit = {
        alpha = (alpha + math.max(0, reward)) * (1-recencyBias) + 1.0 * recencyBias
        beta = (beta - math.min(0, reward)) * (1-recencyBias)  + 1.0 * recencyBias
        betaDistribution = Beta(alpha, beta)
    }

    def export: Json = Json.fromFields(Map(
        "alpha" -> Json.fromDouble(alpha).get,
        "beta" -> Json.fromDouble(beta).get
    ))
}


class ExpDouble(private var value: Double) extends SimpleDistribution {
    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {value = reward; ()}
}
object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble(d) 
}


class Exp3Reward(private var value: Double, gamma: Double, k: Int) extends SimpleDistribution{
    def draw: Double = value
    def update(reward: Reward): Unit = {
        value = value * math.exp(gamma* reward/(k))
    }
    def export: Json = Json.fromFields(Map(
        "value" -> Json.fromDouble(value).get,
        "gamma" -> Json.fromDouble(gamma).get,
        "k" -> Json.fromInt(k)
    ))
}
