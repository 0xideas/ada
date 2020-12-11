package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


abstract class GreedyWithContextAbstract
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ContextualDistribution[Context]]
    (models: ModelID  => Model[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends ContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with ContextualActor[ModelID, ModelData, ModelAction]{

    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID) = {
        _actImpl[Context, AggregateReward](models, modelKeys, modelRewards, epsilon, data, context)
    }

    def update(modelId: ModelID, context: Context, reward: Reward): Unit = 
        modelRewards(modelId).update(context, reward)
}

class GreedyWithContext
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ContextualDistribution[Context]]
    (models: ModelID  => Model[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyWithContextAbstract[ModelID, Context, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with Greedy[ModelID, ModelData, ModelAction]

