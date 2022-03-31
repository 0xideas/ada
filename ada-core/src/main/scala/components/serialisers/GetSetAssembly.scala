package ada.enhancements

import ada.interface.{AdaEnsemble, Model}
import ada.components.distributions.Distribution
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.apache.commons.math3.analysis.function.Exp
import ada.components.distributions.Distribution
import ada.components.distributions.BetaDistribution
import ada.interface.InertModel
import ada.components.learners.BayesianLinearRegressionAbstract
import ada.components.distributions._
import ada.models.OnnxRegression
import ada.models.OnnxClassifier
import ada.models.GenericStaticModel
import ada.assemblies._
import ada.`package`.Reward
import scala.collection.immutable.ListMap
import ada.interface._
import ada.components.selectors._

class GetSetAssembly[ModelID, ModelData, ModelAction, AggregateReward <: Distribution with Updateable](
        implicit modelIdDecoder: Decoder[ModelID],
        modelIdEncoder: Encoder[ModelID],
        modelIdOrd: Ordering[ModelID],
        modelActionDecoder: Decoder[ModelAction], 
        modelActionEncoder: Encoder[ModelAction]){

    type Ensemble = AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    
    private case class AssemblyParameters[ModelID](ensembles: List[Json])
    private implicit val updateDecoderAssembly: Decoder[AssemblyParameters[ModelID]] = deriveDecoder[AssemblyParameters[ModelID]]
    private implicit val updateEncoderAssembly: Encoder[AssemblyParameters[ModelID]] = deriveEncoder[AssemblyParameters[ModelID]]
    
    val getSetModel = new GetSetModel[ModelID, ModelData, ModelAction, AggregateReward]
    import getSetModel.{getModelParameters, setModelParameters}

    val getSetTree = new GetSetTree[ModelAction]; import getSetTree.{treeEncoder, treeDecoder}

    type AssemblyT = Assembly[ModelID, ModelData, ModelAction, AggregateReward]

    implicit def getAssemblyParameters(assembly: AssemblyT): Json = {
        val ensembles: List[Json] = assembly.ensembles.map{ensemble => getModelParameters(ensemble)}.toList
        AssemblyParameters(ensembles).asJson
    }

    
    implicit def setAssemblyParameters(assembly: AssemblyT, parameters: Json): Unit = {
        val pars = parameters.as[AssemblyParameters[ModelID]]
        pars match {
            case Right(AssemblyParameters(ensembles)) => {
                ensembles.zipWithIndex.map{case(parameters, depth) => setModelParameters(assembly.ensembles(depth), parameters )}
            }
            case Left(decodingFailure) => println(decodingFailure)
        }
    }

    
    def buildExportAssemblyParameters()(
        implicit modelEncoder: Encoder[StackableModel[ModelID, ModelData, ModelAction]],
        modelDecoder: Decoder[StackableModel[ModelID, ModelData, ModelAction]]
        ): (Encoder[StackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward]], Decoder[StackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward]]) = {
        type CustomAssembly = StackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward]
        implicit val encodeAssembly: Encoder[CustomAssembly] = new Encoder[CustomAssembly] {
            final def apply(assembly: CustomAssembly): Json = {
                Json.fromFields(List(("StackableAssembly1", Json.fromValues(assembly.getEnsembles.map{model =>  modelEncoder(model)}))))
            }
        }
        implicit val decodeAssembly: Decoder[CustomAssembly] = new Decoder[CustomAssembly] {
            final def apply(c: HCursor): Decoder.Result[CustomAssembly] = {
                for {
                    modelsJson <- c.downField("StackableAssembly1").as[List[Json]]
                } yield {
                    val models = modelsJson.map{modelJson =>  modelDecoder(modelJson.hcursor)}
                                           .map{
                                               case(Right(model)) => model
                                               case(Left(error)) => throw error
                                           }
                    new StackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward](models)
            
                }                
            }
        }
        val l = List((1, "hello"),(2, "goodbye"))
        Json.fromValues(l.map(ll => Json.fromFields(List(("int", Json.fromInt(ll._1)), ("word", Json.fromString(ll._2))))))
        (encodeAssembly, decodeAssembly)
    }
}
