package ada.core.interface

import scala.collection.mutable.{Map => MutableMap}
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.distributions._

//not used at the moment
trait PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{
    def _updateAllImpl(data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward,
                       update: (ModelID, ModelAction, ModelAction) => Unit): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update(id, modelAction, optimalAction) 
            }
        } 
    }
    def _updateAllImpl[Context]
                       (data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward,
                       update: (ModelID, Context, ModelAction, ModelAction) => Unit,
                       context: Context): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update(id, context, modelAction, optimalAction) 
            }
        } 
    }

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
}

trait ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: Exportable]{
    def export(models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
               modelKeys: () => List[ModelID],
               modelRewards: MutableMap[ModelID, AggregateReward]): Json = Json.fromFields(Map(
        "models" -> Json.fromFields(modelKeys().map{
            id => (id.toString(), models(id).export)
        }),
        "modelRewards" -> Json.fromFields(modelRewards.map{
            case(id, aggReward) => (id.toString(), aggReward.export)
        })
    ))
}

