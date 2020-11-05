package epsilon.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.learners._
import epsilon.core.components.distributions.ContextualDistribution



class EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: AggregateReward => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward)
    extends PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction]
    with EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, AggregateReward]{

    def actWithID(data: ModelData): (ModelAction, ModelID) =
    	_actImpl(models, modelRewards, draw, epsilon, data)
    //def act[Context](context: Context, data: ModelData): ModelAction = act(data)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward =
    	evaluationFn(action, optimalAction)

    def updateAll(data: ModelData, correct: ModelAction): Unit = 
    	_updateAllImpl(data, correct, models, modelRewards, this.update)
    
    def getModelRewards: MutableMap[ModelID, AggregateReward] = modelRewards

    def update(modelId: ModelID, reward: Reward): Unit = 
    	updateFn(modelId, reward, modelRewards,  updateAggregateRewardFn)

}

object EpsilonEnsembleGreedySoftmaxLocal{
    def apply[ModelID, ModelData, ModelAction, AggregateReward](
        models: Map[ModelID, Model[ModelData, ModelAction]],
        initAggregateRewards: AggregateReward,
        draw: AggregateReward => Double,
        epsilon: Double,
        evaluationFn: (ModelAction, ModelAction) => Reward,
        updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward){
            val modelRewards = MutableMap(models.toSeq.map{case(k,v) => (k, initAggregateRewards)}:_*)
            new EpsilonEnsembleGreedySoftmaxLocal
                [ModelID, ModelData, ModelAction, AggregateReward](
                models, modelRewards, draw, epsilon, evaluationFn, updateAggregateRewardFn)
        }
}


class EpsilonEnsembleGreedySoftmaxLocalWithContext
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ContextualDistribution[Context, Reward]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: (Context, AggregateReward) => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward)
    extends PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction]
    with EpsilonEnsembleWithContext[ModelID, Context, ModelData, ModelAction, AggregateReward]{

    //def update[Context](modelId: ModelID, context: Context, reward: Reward): Unit = update(modelId, context, reward)
    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID) =
    	_actImpl[Context](models, modelRewards, context, draw, epsilon, data)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = 
    	evaluationFn(action, optimalAction)

    def updateAll(context: Context, data: ModelData, correct: ModelAction): Unit = 
    	_updateAllImpl[Context](context, data, correct, models, modelRewards, this.update)
    
    def getModelRewards: MutableMap[ModelID, AggregateReward] = modelRewards

    def update(modelId: ModelID, context: Context, reward: Reward): Unit = 
    	updateFn[Context, AggregateReward](
                modelId, context, reward, modelRewards: ModelID => AggregateReward)

}