package epsilon.interfaces

import scala.collection.mutable.{Map => MutableMap}

import epsilon._

trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
}


trait EpsilonEnsembleActive[ModelID, ModelData, ModelAction, AggregateReward]
    extends Model[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelID, reward: Reward): Unit
    def update(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, evaluate(action, optimalAction))
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
}

trait LocalEnsemble[ModelID, ModelData, ModelAction, AggregateReward] {
    def updateFn(modelId: ModelID, reward: Reward, modelRewardsMap: MutableMap[ModelID,AggregateReward],
                    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward) = {
        val oldReward = modelRewardsMap(modelId)
        val newReward =  updateAggregateRewardFn(oldReward, reward)
        modelRewardsMap(modelId) = newReward
    }
}
