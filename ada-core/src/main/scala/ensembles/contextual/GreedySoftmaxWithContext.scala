package ada.core.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class GreedySoftmaxWithContext
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ContextualDistribution[Context]]
    (models: ModelID  => SimpleModel[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyWithContextAbstract[ModelID, Context, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with GreedySoftmax[ModelID, ModelData, ModelAction]
