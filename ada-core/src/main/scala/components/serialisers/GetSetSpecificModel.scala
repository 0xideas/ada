package ada.models

import ada.interface.InertModel
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.Json
import io.circe.parser.decode

import ada.interface.Model
import ada.models.BayesianLinearRegressionModelAbstract
import ada.components.learners.BayesianLinearRegressionAbstract

import breeze.stats.distributions.{Beta, Bernoulli}
import breeze.linalg._
import breeze.numerics._

import ada.components.learners._
import ada.models._

class GetSetSpecificModel[ModelID, ModelData, ModelAction, AggregateReward](
        implicit modelActionEncoder: Encoder[ModelAction],
        modelActionDecoder: Decoder[ModelAction]
    ){

    def f[AltModelData, AltModelAction](model: Model[AltModelData,AltModelAction]): Model[ModelData,ModelAction] = {
        model match {
            case(m : Model[ModelData, ModelAction]) => m
            case(m) => throw new Exception(m.toString + " cannot be reconstructed")
        }
    }

    //GenericStaticModel
    private case class GenericStaticModelParameters[ModelAction](kind: String, value: ModelAction)
    private implicit val GenericStaticModelParameterDecoder: Decoder[GenericStaticModelParameters[ModelAction]] = deriveDecoder[GenericStaticModelParameters[ModelAction]]
    private implicit val GenericStaticModelParameterEncoder: Encoder[GenericStaticModelParameters[ModelAction]] = deriveEncoder[GenericStaticModelParameters[ModelAction]]


    def setGenericStaticModel(model: GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward], parameters: Json): Unit = {
        val pars = parameters.as[GenericStaticModelParameters[ModelAction]]
        pars match {
            case Right(GenericStaticModelParameters("GenericStaticModel", valueV)) => {model.value = valueV}
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }

    def exportGenericStaticModel(model: GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward]): Json = {
        GenericStaticModelParameters("GenericStaticModel", model.value).asJson
    } 
    

    def reconstructGenericStaticModel(parameters: Json): GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward] = {
        val pars = parameters.as[GenericStaticModelParameters[ModelAction]]
        pars match {
            case Right(GenericStaticModelParameters("GenericStaticModel", valueV)) => new GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward](valueV)
            case(Right(_)) => throw new Exception("Decoding of GenericStaticModel failed")
            case Left(_) => throw new Exception("Decoding of GenericStaticModel failed")
        }
    }


    def setInertModel(model: InertModel[ModelID, ModelData, ModelAction], parameters: Json): Unit = ()

    def exportOnnxRegression(model: OnnxRegression[ModelID, ModelData, ModelAction, AggregateReward]): Json = {
        Json.fromFields(List(("OnnxRegression", Json.fromString(model.loadingTime + "   " + model.path))))
    }
    def exportOnnxClassifier(model: OnnxClassifier[ModelID, ModelData, ModelAction, AggregateReward]): Json = {
        Json.fromFields(List(("OnnxClassifier", Json.fromString(model.loadingTime + "   " + model.path))))
    }


    case class BayesianRegressionModelParameters(kind: String, nfeatures: Int, alpha: Double, beta: Double, mean: Array[Double], covInv: Array[Double])
    implicit val BayesianRegressionModelParametersDecoder: Decoder[BayesianRegressionModelParameters] = deriveDecoder[BayesianRegressionModelParameters]
    implicit val BayesianRegressionModelParametersEncoder: Encoder[BayesianRegressionModelParameters] = deriveEncoder[BayesianRegressionModelParameters]

    implicit def setBayesianRegressionModel(distribution: BayesianLinearRegressionAbstract, parameters: Json): Unit = {
        val pars = parameters.as[BayesianRegressionModelParameters]
        pars match {
            case Right(BayesianRegressionModelParameters(regressionType, nfeaturesV, alphaV, betaV, meanV, covInvV)) => {
                distribution.nfeatures = nfeaturesV
                distribution.alpha = alphaV
                distribution.beta = betaV
                distribution.setMean(DenseVector(meanV))
                distribution.setCovInv(DenseMatrix(covInvV).reshape(distribution.nfeatures, distribution.nfeatures))

            }
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }
    def bayesianLinearRegressionModelType(distribution: BayesianLinearRegressionAbstract): String = {
        distribution match {
            case _: BayesianMeanRegressionModel[ModelID, AggregateReward] => "BayesianMeanRegressionModel"
            case _: BayesianSampleRegressionModel[ModelID, AggregateReward] => "BayesianSampleRegressionModel"
        }
    }
    implicit def exportBayesianRegressionModel(distribution: BayesianLinearRegressionAbstract): Json = {
        BayesianRegressionModelParameters(bayesianLinearRegressionModelType(distribution), distribution.nfeatures, distribution.alpha, distribution.beta, distribution.mean.toArray, distribution.covInv.toArray).asJson
    }

    def reconstructBayesianLinearRegressionModel(parameters: Json): BayesianLinearRegressionModelAbstract[ModelID, AggregateReward] = {
        val pars = parameters.as[BayesianRegressionModelParameters]
        pars match {
            case Right(BayesianRegressionModelParameters("BayesianMeanRegressionModel", nfeaturesV, alphaV, betaV, meanV, covInvV)) => {val model= new BayesianMeanRegressionModel[ModelID, AggregateReward](nfeaturesV, alphaV, betaV); setBayesianRegressionModel(model, parameters); model}
            case Right(BayesianRegressionModelParameters("BayesianSampleRegressionModel", nfeaturesV, alphaV, betaV, meanV, covInvV)) => {val model= new BayesianSampleRegressionModel[ModelID, AggregateReward](nfeaturesV, alphaV, betaV); setBayesianRegressionModel(model, parameters); model}
            case Left(decodingFailure) => throw new Exception("BayesianLinearRegression could not be decoded")
        }
    }


    def setBayesianLinearRegression(distribution: BayesianLinearRegressionAbstract, parameters: Json): Unit = {
        val pars = parameters.as[BayesianRegressionModelParameters]
        pars match {
            case Right(BayesianRegressionModelParameters(_, nfeaturesV, alphaV, betaV, meanV, covInvV)) => {
                distribution.nfeatures = nfeaturesV
                distribution.alpha = alphaV
                distribution.beta = betaV
                distribution.setMean(DenseVector(meanV))
                distribution.setCovInv(DenseMatrix(covInvV).reshape(distribution.nfeatures, distribution.nfeatures))

            }
            case Left(decodingFailure) => {println(parameters); println(decodingFailure)}
        }
    }

    //Models
    implicit def getSpecificModelParameters(model: Model[ModelData, ModelAction]): Json = {
        model match {
            case(m: OnnxRegression[ModelID, ModelData, ModelAction, AggregateReward]) => exportOnnxRegression(m)
            case(m: OnnxClassifier[ModelID, ModelData, ModelAction, AggregateReward]) => exportOnnxClassifier(m)
            case(m: GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward]) => exportGenericStaticModel(m)
            case(m: BayesianMeanRegressionModel[ModelID, AggregateReward]) => exportBayesianRegressionModel(m)
            case(m: BayesianSampleRegressionModel[ModelID, AggregateReward]) => exportBayesianRegressionModel(m)
            case(_) => Json.fromString("This model cannot be serialised.")
        }
    }

    implicit def setSpecificModelParameters(model: Model[ModelData, ModelAction], parameters: Json): Unit = {
        model match {
            case(model: BayesianMeanRegressionModel[ModelID, AggregateReward]) => setBayesianLinearRegression(model, parameters)
            case(model: BayesianSampleRegressionModel[ModelID, AggregateReward]) => setBayesianLinearRegression(model, parameters)
            case(model: GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward]) => setGenericStaticModel(model, parameters)
            case(model: InertModel[ModelID, ModelData, ModelAction]) => setInertModel(model, parameters)
            case(_) => ()
        }
    }

    def reconstructModel(parameters: Json): Model[ModelData, ModelAction] = {
        val cursor = parameters.hcursor
        val model = cursor.downField("kind").as[String] match {
            case(Right("GenericStaticModel")) => reconstructGenericStaticModel(parameters)
            case(Right("BayesianMeanRegressionModel")) => reconstructBayesianLinearRegressionModel(parameters)
            case(Right("BayesianSampleRegressionModel")) => reconstructBayesianLinearRegressionModel(parameters)
            case(Right(_)) =>  throw new Exception("None of the available models (GenericStaticModel, BayesianLinearRegression) was selected.")
            case(Left(_)) => throw new Exception("Serialised model could not be decoded.")
        }
        f(model)
    }

}