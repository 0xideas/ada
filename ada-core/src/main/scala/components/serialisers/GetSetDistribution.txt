package ada.components.distributions

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.Json
import io.circe.parser.decode

import breeze.stats.distributions.{Beta, Bernoulli}
import breeze.linalg._
import breeze.numerics._

import ada.components.learners.BayesianLinearRegressionAbstract
import ada.components.learners.BayesianMeanLinearRegression
import ada.components.learners.BayesianSampleLinearRegression

import ada.models.{BayesianMeanRegressionModel, BayesianSampleRegressionModel}

class GetSetDistribution[AggregateReward]{

    def f(distribution: Distribution): AggregateReward = {
        distribution match {
            case(distr : AggregateReward) => distr
            case(_) => throw new Exception("Reward distribution cannot be decoded.")
        }
    }

    def reconstructReward(parameters: Json): AggregateReward = {
        val cursor = parameters.hcursor
        val distribution = cursor.downField("kind").as[String] match {
            case(Right("Exp3Reward")) => reconstructExp3Reward(parameters)
            case(Right("BetaDistribution")) => reconstructBetaDistribution(parameters)
            case(Right("MeanDouble")) =>  reconstructMeanDouble(parameters)
            case(Right("ExpDouble")) => reconstructExpDouble(parameters)
            case(Right("BayesianLinearRegression")) => reconstructBayesianLinearRegression(parameters)
            case(Right(_)) => throw new Exception("None of the available rewards (Exp3Reward, BetaDistribution, MeanDouble, ExpDouble) was selected.")
            case(Left(_)) => throw new Exception("Reward did not have field kind indicating the type of the reward.")
        }
        f(distribution)
    }

    //MeanDouble
    private case class MeanDoubleParameters(kind: String, value: Double, i: Int)
    private implicit val MeanDoubleParametersDecoder: Decoder[MeanDoubleParameters] = deriveDecoder[MeanDoubleParameters]
    private implicit val MeanDoubleParametersEncoder: Encoder[MeanDoubleParameters] = deriveEncoder[MeanDoubleParameters]

    def setMeanDouble(distribution: MeanDouble, parameters: Json): Unit = {
        val pars = parameters.as[MeanDoubleParameters]
        pars match {
            case Right(MeanDoubleParameters("MeanDouble", valueV, iV)) => {distribution.value = valueV; distribution.i = iV}
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }

    def exportMeanDouble(distribution: MeanDouble): Json = MeanDoubleParameters("MeanDouble", distribution.value, distribution.i.toInt).asJson

    def reconstructMeanDouble(parameters: Json): MeanDouble = {
        val pars = parameters.as[MeanDoubleParameters]
        pars match {
            case Right(MeanDoubleParameters("MeanDouble", valueV, iV)) => {val meanDouble = new MeanDouble(); setMeanDouble(meanDouble, parameters); meanDouble}
            case Left(decodingFailure) => throw new Exception("MeanDouble could not be decoded")
        }
    }
    //BetaDistribution
    private case class BetaDistributionParameters(kind: String, alpha: Double, beta: Double)
    private implicit val BetaDistributionParametersDecoder: Decoder[BetaDistributionParameters] = deriveDecoder[BetaDistributionParameters]
    private implicit val BetaDistributionParametersEncoder: Encoder[BetaDistributionParameters] = deriveEncoder[BetaDistributionParameters]
    implicit def exportBetaDistribution(distribution: BetaDistribution): Json = {
        BetaDistributionParameters("BetaDistribution", distribution.alpha, distribution.beta).asJson
    }

    implicit def setBetaDistribution(betaDistribution: BetaDistribution, parameters: Json): Unit = {
        val pars = parameters.as[BetaDistributionParameters]
        pars match {
            case Right(BetaDistributionParameters("BetaDistribution", alphaV, betaV)) => {
                betaDistribution.alpha = alphaV
                betaDistribution.beta = betaV
                betaDistribution.betaDistribution = Beta(alphaV, betaV)
            }
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }

    def reconstructBetaDistribution(parameters: Json): BetaDistribution = {
        val pars = parameters.as[BetaDistributionParameters]
        pars match {
            case Right(BetaDistributionParameters("BetaDistribution",alpha, beta)) => new BetaDistribution(alpha, beta)
            case Left(decodingFailure) => throw new Exception("BetaDistribution could not be decoded")
        }
    }

    //Exp3Reward
    private case class Exp3RewardParameters(kind: String, value: Double, gamma: Double, k: Int)
    private implicit val Exp3RewardParametersDecoder: Decoder[Exp3RewardParameters] = deriveDecoder[Exp3RewardParameters]
    private implicit val Exp3RewardParametersEncoder: Encoder[Exp3RewardParameters] = deriveEncoder[Exp3RewardParameters]
    implicit def exportExp3Reward(distribution: Exp3Reward): Json = {
        Exp3RewardParameters("Exp3Reward", distribution.value, distribution.gamma, distribution.k).asJson
    }

    implicit def setExp3Reward(distribution: Exp3Reward, parameters: Json): Unit = {
        val pars = parameters.as[Exp3RewardParameters]
        pars match {
            case Right(Exp3RewardParameters("Exp3Reward", valueV, gammaV, kV)) => {distribution.value = valueV; distribution.gamma = gammaV; distribution.k = kV;}
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }

    def reconstructExp3Reward(parameters: Json): Exp3Reward = {
        val pars = parameters.as[Exp3RewardParameters]
        pars match {
            case Right(Exp3RewardParameters("Exp3Reward", valueV, gammaV, kV)) => new Exp3Reward(valueV, gammaV, kV)
            case Left(decodingFailure) => throw new Exception("Exp3Reward could not be decoded")
        }
    }


    private case class ExpDoubleParameters(kind: String, value: Double)
    private implicit val ExpDoubleParametersDecoder: Decoder[ExpDoubleParameters] = deriveDecoder[ExpDoubleParameters]
    private implicit val ExpDoubleParametersEncoder: Encoder[ExpDoubleParameters] = deriveEncoder[ExpDoubleParameters]
    //ExpDouble
    implicit def exportExpDouble(distribution: ExpDouble): Json = {
        ExpDoubleParameters("ExpDouble", distribution.value).asJson
    }

    implicit def setExpDouble(distribution: ExpDouble, parameters: Json): Unit = {
        decode[Double](parameters.toString) match {
            case Right(valueV: Double) => {distribution.value = valueV; ()}
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }

    def reconstructExpDouble(parameters: Json): ExpDouble = {
        val pars = decode[Double](parameters.toString)
        pars match {
            case Right(valueV) => new ExpDouble(valueV)
            case Left(decodingFailure) => throw new Exception("ExpDouble could not be decoded")
        }
    }


    //BayesianLinearRegressionAbstract
    private case class BayesianLinearRegressionParameters(regressionType: String, nfeatures: Int, alpha: Double, beta: Double, mean: Array[Double], covInv: Array[Double])
    private implicit val BayesianLinearRegressionParametersDecoder: Decoder[BayesianLinearRegressionParameters] = deriveDecoder[BayesianLinearRegressionParameters]
    private implicit val BayesianLinearRegressionParametersEncoder: Encoder[BayesianLinearRegressionParameters] = deriveEncoder[BayesianLinearRegressionParameters]

    implicit def setBayesianLinearRegression(distribution: BayesianLinearRegressionAbstract, parameters: Json): Unit = {
        val pars = parameters.as[BayesianLinearRegressionParameters]
        pars match {
            case Right(BayesianLinearRegressionParameters(_, nfeaturesV, alphaV, betaV, meanV, covInvV)) => {
                distribution.nfeatures = nfeaturesV
                distribution.alpha = alphaV
                distribution.beta = betaV
                distribution.setMean(DenseVector(meanV))
                distribution.setCovInv(DenseMatrix(covInvV).reshape(distribution.nfeatures, distribution.nfeatures))

            }
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }
    def bayesianLinearRegressionType(distribution: BayesianLinearRegressionAbstract): String = {
        distribution match {
            case _: BayesianMeanLinearRegression => "mean"
            case _: BayesianSampleLinearRegression => "sample"
        }
    }

    implicit def exportBayesianLinearRegression(distribution: BayesianLinearRegressionAbstract): Json = {
        BayesianLinearRegressionParameters(bayesianLinearRegressionType(distribution),
                                            distribution.nfeatures, distribution.alpha,
                                            distribution.beta,
                                            distribution.mean.toArray,
                                            distribution.covInv.toArray).asJson
    }

    def reconstructBayesianLinearRegression(parameters: Json): BayesianLinearRegressionAbstract = {
        val pars = parameters.as[BayesianLinearRegressionParameters]
        pars match {
            case Right(BayesianLinearRegressionParameters("mean", nfeaturesV, alphaV, betaV, meanV, covInvV)) =>{val model= new BayesianMeanLinearRegression(nfeaturesV, alphaV, betaV); setBayesianLinearRegression(model, parameters); model}
            case Right(BayesianLinearRegressionParameters("sample", nfeaturesV, alphaV, betaV, meanV, covInvV)) => {val model= new BayesianSampleLinearRegression(nfeaturesV, alphaV, betaV); setBayesianLinearRegression(model, parameters); model}
            case Left(decodingFailure) => throw new Exception("BayesianLinearRegression could not be decoded")
        }
    }

    //Rewards
    implicit def exportReward(reward: Distribution): Json = {
        reward match {
            case(r: BetaDistribution) => exportBetaDistribution(r)
            case(r: Exp3Reward) => exportExp3Reward(r)
            case(r: ExpDouble) => exportExpDouble(r)
            case(r: MeanDouble) => exportMeanDouble(r)
            case(r: BayesianLinearRegressionAbstract) => exportBayesianLinearRegression(r)
            case(_) => Json.fromString("This reward cannot be serialised")
        }
    }

    implicit def setReward(reward: Distribution, parameters: Json): Unit = {
        reward match {
            case(distribution: BetaDistribution) => setBetaDistribution(distribution, parameters)
            case(distribution: Exp3Reward) => setExp3Reward(distribution, parameters)
            case(distribution: ExpDouble) => setExpDouble(distribution, parameters)
            case(distribution: MeanDouble) => setMeanDouble(distribution, parameters)
            case(distribution: BayesianLinearRegressionAbstract) => setBayesianLinearRegression(distribution, parameters)
            case(_) => ()
        }
    }
}