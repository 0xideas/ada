package ada.components.distributions

import breeze.stats.distributions.{Beta, Bernoulli}
import io.circe.Json

import ada._
import ada.interface.Settable
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser.decode

class BetaDistribution (private var alpha: Double, private var beta: Double, learningRate: Double = 1.0)
    extends SimpleDistribution
    with Settable{
    private var betaDistribution = Beta(alpha, beta)
    override def toString: String = {
        f"alpha: $alpha beta: $beta"
    }

    private case class Update(alpha: Double, beta: Double)
    private implicit val updateDecoder: Decoder[Update] = deriveDecoder[Update]
    private implicit val updateEncoder: Encoder[Update] = deriveEncoder[Update]

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

    def export: Json = Update(alpha, beta).asJson

    def setParameters(parameters: Json): Unit = {
        val pars = parameters.as[Update]
        pars match {
            case Right(Update(alphaV, betaV)) => {
                alpha = alpha
                beta = betaV
                betaDistribution = Beta(alpha, beta)
            }
            case Left(decodingFailure) => println(decodingFailure)
        }
    }
}



class MeanDouble extends SimpleDistribution with Settable{
    private var i = 1.0
    private var value = 0.0


    def draw: Double = value
    def update(reward: Reward): Unit = {
        val oldValue = value
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            value = value*(1.0-1.0/i) + reward.value * (1.0/i)
            i+=1.0
        }
    }

    def export: Json =  Json.fromDouble(value).get

    def setParameters(parameters: Json): Unit = {
        decode[Double](parameters.toString) match {
            case Right(valueV) => {value = valueV; ()}
            case Left(decodingFailure) => println(decodingFailure)
        }
    }
}



class Exp3Reward(private var value: Double, private var gamma: Double, private var k: Int) extends SimpleDistribution{

    private case class Update(value: Double, gamma: Double, k: Int)
    private implicit val updateDecoder: Decoder[Update] = deriveDecoder[Update]
    private implicit val updateEncoder: Encoder[Update] = deriveEncoder[Update]

    def draw: Double = value

    def update(reward: Reward): Unit = {
        if(!(reward.value.isInfinite || reward.value.isNaN() )){
            value = value * math.exp(gamma* reward.value/(k))
        }
    }

    def export: Json = Update(value, gamma, k).asJson

    def setParameters(parameters: Json): Unit = {
        val pars = parameters.as[Update]
        pars match {
            case Right(Update(valueV, gammaV, kV)) => {value = valueV; gamma = gammaV; k = kV;}
            case Left(decodingFailure) => println(decodingFailure)
        }
    }

}



//TEST

class ExpDouble(private var value: Double) extends SimpleDistribution {
    private case class Update(value: Double)

    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {if(!(reward.value.isInfinite || reward.value.isNaN() )){value = reward.value}; ()}


    def setParameters(parameters: Json): Unit = {
        decode[Double](parameters.toString) match {
            case Right(valueV) => {value = valueV; ()}
            case Left(decodingFailure) => println(decodingFailure)
        }
    }
}

object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble( d)
}