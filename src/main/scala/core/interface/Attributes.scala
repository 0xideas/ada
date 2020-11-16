package epsilon.core.interface

import scala.collection.mutable.{Map => MutableMap}
import io.circe.Json

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.distributions._

trait LocalEnsemble[ModelID, ModelData, ModelAction] {
    def _updateFn[AggregateReward]
                (modelRewardsMap: MutableMap[ModelID,AggregateReward],
                modelId: ModelID,
                reward: Reward,
                updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward) = {
        val oldReward = modelRewardsMap(modelId)
        val newReward =  updateAggregateRewardFn(oldReward, reward)
        modelRewardsMap(modelId) = newReward
    }
    def _updateFn[Context, AggregateReward <: ContextualDistribution[Context]]
                (modelRewards: ModelID => AggregateReward,
                modelId: ModelID,
                context: Context,
                reward: Reward) = {
        modelRewards(modelId).update(context, reward)
    }
}

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
                       (context: Context,
                       data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward,
                       update: (ModelID, Context, ModelAction, ModelAction) => Unit): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update(id, context, modelAction, optimalAction) 
            }
        } 
    }
}

trait ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: Exportable]{
    def export(models: Map[ModelID,  Model[ModelData, ModelAction]],
               modelRewards: MutableMap[ModelID, AggregateReward]): Json = Json.fromFields(Map(
        "models" -> Json.fromFields(models.map{
            case(id, model) => (id.toString(), model.export)
        }),
        "modelRewards" -> Json.fromFields(modelRewards.map{
            case(id, aggReward) => (id.toString(), aggReward.export)
        })
    ))
}
