package epsilon.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.learners._
import epsilon.core.components.distributions._


class GreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward <: Exportable]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: AggregateReward => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward)
    extends EnsembleNoContext[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward =
        evaluationFn(action, optimalAction)

    def actWithID(data: ModelData): (ModelAction, ModelID) =
    	_actImpl(models, modelRewards, draw, epsilon, data)

    def updateAll(data: ModelData, correct: ModelAction): Unit = 
    	_updateAllImpl(data, correct, models, modelRewards, this.update)

    def update(modelId: ModelID, reward: Reward): Unit = 
        _updateFn(modelRewards, modelId, reward, updateAggregateRewardFn)

    def export = export(models, modelRewards)
    

}


object GreedySoftmaxLocal{
    def apply[ModelID, ModelData, ModelAction, AggregateReward <: Exportable](
        models: Map[ModelID, Model[ModelData, ModelAction]],
        initAggregateRewards: AggregateReward,
        draw: AggregateReward => Double,
        epsilon: Double,
        evaluationFn: (ModelAction, ModelAction) => Reward,
        updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward){
            val modelRewards = MutableMap(models.toSeq.map{case(k,v) => (k, initAggregateRewards)}:_*)
            new GreedySoftmaxLocal
                [ModelID, ModelData, ModelAction, AggregateReward](
                models, modelRewards, draw, epsilon, evaluationFn, updateAggregateRewardFn)
        }
}


class GreedySoftmaxLocalWithContext
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ContextualDistribution[Context]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: (Context, AggregateReward) => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (Context, AggregateReward, Reward) => AggregateReward)
    extends EnsembleWithContext[ModelID, Context, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = 
        evaluationFn(action, optimalAction)

    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID) =
    	_actImpl[Context](models, modelRewards, context, draw, epsilon, data)

    def updateAll(context: Context, data: ModelData, correct: ModelAction): Unit = 
    	_updateAllImpl[Context](context, data, correct, models, modelRewards, this.update)

    def update(modelId: ModelID, context: Context, reward: Reward): Unit = 
    	_updateFn[Context, AggregateReward](
                 modelRewards: ModelID => AggregateReward, modelId, context, reward)

    def export: Json = export(models, modelRewards)
}

