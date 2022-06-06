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
import ada.models.GetSetSpecificModel

class GetSetModel[ModelID, ModelData, ModelAction, AggregateReward <: Distribution with Updateable](
        implicit modelIdDecoder: Decoder[ModelID],
        modelIdEncoder: Encoder[ModelID],
        modelIdOrd: Ordering[ModelID],
        modelActionDecoder: Decoder[ModelAction], 
        modelActionEncoder: Encoder[ModelAction]){


    val getSetEnsemble = new GetSetEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    import getSetEnsemble.{getEnsembleParameters, setEnsembleParameters, buildExportEnsembleParameters}

    val getSetSpecificModel = new GetSetSpecificModel[ModelID, ModelData, ModelAction, AggregateReward]
    import getSetSpecificModel.{setSpecificModelParameters, getSpecificModelParameters}


    def setModelParameters(model: StackableModel[ModelID, ModelData, ModelAction], parameters: Json): Unit = {
        model match {
            case ensemble: AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward] => setEnsembleParameters(ensemble, parameters)
            case assembly: Assembly[ModelID, ModelData, ModelAction, AggregateReward] => {
                val getSetAssembly = new GetSetAssembly[ModelID, ModelData, ModelAction, AggregateReward]
                import getSetAssembly.{setAssemblyParameters, getAssemblyParameters}
                setAssemblyParameters(assembly, parameters)
            } 
            case _ => setSpecificModelParameters(model, parameters)
        }
    }

    def getModelParameters(model: StackableModel[ModelID, ModelData, ModelAction]): Json = {
        model match {
            case ensemble: AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward] => getEnsembleParameters(ensemble)
            case assembly: Assembly[ModelID, ModelData, ModelAction, AggregateReward] => {
                val getSetAssembly = new GetSetAssembly[ModelID, ModelData, ModelAction, AggregateReward]
                import getSetAssembly.{setAssemblyParameters, getAssemblyParameters}
                getAssemblyParameters(assembly)
            } 
            case _ => getSpecificModelParameters(model)
        }
    }
}