package epsilon.interfaces

import scala.collection.mutable.{Map => MutableMap}

import epsilon._
import epsilon.distributions.ContextualDistribution

trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
    def report: String = this.toString
}


trait EpsilonEnsembleActive[ModelID, ModelData, ModelAction, AggregateReward]
    extends Model[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelID, reward: Reward): Unit
    def update(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, evaluate(action, optimalAction))
    def update[Context](modelId: ModelID, context: Context, reward: Reward): Unit
    def update[Context](modelId: ModelID, context: Context, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, context, evaluate(action, optimalAction))
    def act(data: ModelData): ModelAction = actWithID(data)._1
}


trait EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsembleActive[ModelID, ModelData, ModelAction, AggregateReward]{
    def updateAll(data: ModelData,
            optimalAction: ModelAction): Unit

    def _updateAllImpl(data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update(id, modelAction, optimalAction) 
            }
        } 
    }
    def _updateAllImpl[Context](
                       context: Context,
                       data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update[Context](id, context, modelAction, optimalAction) 
            }
        } 
    }
}

trait LocalEnsemble[ModelID, ModelData, ModelAction] {
    def updateFn[AggregateReward](modelId: ModelID, reward: Reward, modelRewardsMap: MutableMap[ModelID,AggregateReward],
                    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward) = {
        val oldReward = modelRewardsMap(modelId)
        val newReward =  updateAggregateRewardFn(oldReward, reward)
        modelRewardsMap(modelId) = newReward
    }
    def updateFn[Context, AggregateReward <: ContextualDistribution[Context, Reward]]
                (modelId: ModelID, context: Context, reward: Reward, modelRewards: ModelID => AggregateReward) = {
        modelRewards(modelId).update(context, reward)
    }
}

object UpdateAll{
    implicit def _updateAllImpl[ModelID, Context, ModelData, ModelAction, AggregateReward](context: Context,
                       data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward)
                       (implicit update: (ModelID, Context, ModelAction, ModelAction) => Unit): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update(id, context, modelAction, optimalAction) 
            }
        } 
    }
    implicit def _updateAllImpl[ModelID, ModelData, ModelAction, AggregateReward](
                       data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, Model[ModelData, ModelAction]],
                       modelRewards: ModelID => AggregateReward)
                       (implicit update: (ModelID, ModelAction, ModelAction) => Unit): Unit = {
        models.map{case(id, model) => {
                val modelAction = model.act(data)
                update(id, modelAction, optimalAction) 
            }
        }
    }
}