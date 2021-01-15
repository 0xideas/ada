package ada.core.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


abstract class GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel1[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with StackableActor1[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
        _actImpl[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

}


class GreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel1[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with Greedy[ModelID, ModelData, ModelAction]

