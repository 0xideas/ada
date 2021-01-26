package ada.core.interface

import scala.collection.mutable.{Map => MutableMap}
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.distributions._
import ada.core.components.selectors.Selector


trait PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]{
    def _updateAllImplSimple(data: ModelData,
                       optimalAction: ModelAction,
                       models: ModelID => SimpleModel[ModelData, ModelAction],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward]): Unit = {
        modelKeys().map{modelId => {
                val model = models(modelId)
                val modelAction = model.act(data)
                val reward = evaluate(modelAction, optimalAction)
                model.update(data, reward)
                modelRewards(modelId).update(reward)
            }
        } 
    }
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

}

trait PassiveContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[Context]]{
    
    def _updateAllImplContextual
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: ModelID => ContextualModelPassive[ModelID, Context, ModelData, ModelAction, AggregateReward],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward],
                       context: Context): Unit = {
        modelKeys().map{modelId => {
                val model = models(modelId)
                val (modelAction, modelIdsOut) = model.actWithID(context, data, modelIds)
                val reward = evaluate(modelAction, optimalAction)
                model.updateAll(List(modelId)++modelIds, context, data, optimalAction)
                modelRewards(modelId).update(context, reward)

            }
        }
    }

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def updateAll(data: ModelData, optimalAction: ModelAction): Unit
}


trait PassiveStackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]{

    def _updateAllImplStackable1
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: ModelID => StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward]): Unit = {

        modelKeys().map{modelId => {
                val model = models(modelId)
                val (modelAction, modelIdsOut) = model.actWithID(data, modelIds)
                val reward = evaluate(modelAction, optimalAction)
                model.updateAll(List(modelId)++modelIds, data, optimalAction)
                modelRewards(modelId).update(reward)
            }
        } 
    }

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit
}


trait PassiveStackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[ModelData]]{

    def _updateAllImplStackable2
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: ModelID => StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward]): Unit = {

        modelKeys().map{modelId => {
                val model = models(modelId)
                val (modelAction, modelIdsOut) = model.actWithID(data, modelIds)
                val reward = evaluate(modelAction, optimalAction)
                model.updateAll(List(modelId)++modelIds, data, optimalAction)
                modelRewards(modelId).update(data, reward)
            }
        } 
    }

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit
}