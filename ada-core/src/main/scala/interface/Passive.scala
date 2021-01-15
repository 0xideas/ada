package ada.core.interface

import scala.collection.mutable.{Map => MutableMap}
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.distributions._
import ada.core.components.selectors.Selector

//not used at the moment
trait PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{
    def _updateAllImplSimple(data: ModelData,
                       optimalAction: ModelAction,
                       models: ModelID => SimpleModel[ModelData, ModelAction],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward],
                       update: (ModelID, Reward) => Unit): Unit = {
        modelKeys().map{modelId => {
                val model = models(modelId)
                val modelAction = model.act(data)
                update(modelId, evaluate(modelAction, optimalAction))
            }
        } 
    }
    def _updateAllImplContextual[Context]
                       (data: ModelData,
                       optimalAction: ModelAction,
                       models: ModelID => ContextualModel[Context, ModelData, ModelAction],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward],
                       update: (ModelID, Context, Reward) => Unit,
                       context: Context): Unit = {
        modelKeys().map{modelId => {
                val model = models(modelId)
                val modelAction = model.act(context, data)
                update(modelId, context, evaluate(modelAction, optimalAction))
            }
        }
    }

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def updateAll(data: ModelData, optimalAction: ModelAction): Unit
}


trait PassiveEnsembleStackable[ModelID, ModelData, ModelAction, AggregateReward]{

    def _updateAllImplStackable
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: ModelID => StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward],
                       modelKeys: () => List[ModelID],
                       modelRewards: Map[ModelID, AggregateReward],
                       update: (List[ModelID], ModelData, Reward) => Unit): Unit = {

        modelKeys().map{modelId => {
                val model = models(modelId)
                val (modelAction, modelIdsOut) = model.actWithID(data, modelIds)
                update(List(modelId) ++ modelIds, data, evaluate(modelAction, optimalAction))
                model.updateAll(modelIds, data, optimalAction)
            }
        } 
    }

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit
}