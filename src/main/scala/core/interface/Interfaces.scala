package epsilon.interfaces

import scala.collection.mutable.{Map => MutableMap}

import epsilon._
import epsilon.distributions.ContextualDistribution

trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
    def act[Context](context: Context, data: ModelData): ModelAction
    def report: String = this.toString
}

trait ContextualModel[ModelData, ModelAction, Context] extends Model[ModelData, ModelAction]{
    override def act(data: ModelData): ModelAction = throw new Exception("No Context provided!!")
}
trait NoContextModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    override def act[Context](context: Context, data: ModelData): ModelAction = throw new Exception("Context should not be provided!!")
}    def actWithID[Context2](context: Context2, data: ModelData): (ModelAction, ModelID) = actWithID(context, data)



trait EpsilonEnsembleActive[ModelID, ModelData, ModelAction, AggregateReward]
    extends Model[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def act(data: ModelData): ModelAction = actWithID(data)._1
    def actWithID[Context](context: Context, data: ModelData): (ModelAction, ModelID)
    def act[Context](context: Context, data: ModelData): ModelAction = actWithID[Context](context, data)._1
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelID, reward: Reward): Unit
    def update(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, evaluate(action, optimalAction))
    def update[Context](modelId: ModelID, context: Context, reward: Reward): Unit
    def update[Context](modelId: ModelID, context: Context, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, context, evaluate(action, optimalAction))

}

trait ContextualEpsilonEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsembleActive[ModelID, ModelData, ModelAction, AggregateReward]
    with ContextualModel[ModelData, ModelAction, Context]{
    def update(modelId: ModelID, reward: Reward): Unit = ()
    def updateAll(modelData:ModelData, modelAction: ModelAction): Unit = ()
    override def actWithID(data: ModelData): (ModelAction, ModelID) = throw new Exception("No Context provided!!")

}

trait NoContextEpsilonEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsembleActive[ModelID, ModelData, ModelAction, AggregateReward]{
    def update[Context](modelId: ModelID, context: Context, reward: Reward): Unit = ()
    override def actWithID[Context](context:Context, data: ModelData): (ModelAction, ModelID) = throw new Exception("No Context provided!!")
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

