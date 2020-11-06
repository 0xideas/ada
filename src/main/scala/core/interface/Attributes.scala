package epsilon.core.interface

import scala.collection.mutable.{Map => MutableMap}

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.distributions._

trait LocalEnsemble[ModelID, ModelData, ModelAction] {
    def updateFn[AggregateReward]
                (modelId: ModelID,
                reward: Reward,
                modelRewardsMap: MutableMap[ModelID,AggregateReward],
                updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward) = {
        val oldReward = modelRewardsMap(modelId)
        val newReward =  updateAggregateRewardFn(oldReward, reward)
        modelRewardsMap(modelId) = newReward
    }
    def updateFn[Context, AggregateReward <: ContextualDistribution[Context]]
                (modelId: ModelID,
                context: Context,
                reward: Reward,
                modelRewards: ModelID => AggregateReward) = {
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
