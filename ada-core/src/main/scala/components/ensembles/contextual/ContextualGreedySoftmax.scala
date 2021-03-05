package ada.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._


class ContextualGreedySoftmax
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ConditionalDistribution[Context]]
    (models: ModelID  => ContextualModel[ModelID, Context, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends ContextualGreedyAbstract[ModelID, Context, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with GreedySoftmax[ModelID, ModelData, ModelAction]{

}