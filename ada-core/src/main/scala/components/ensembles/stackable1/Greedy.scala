package ada.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._


abstract class GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with StackableActor1[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
        _actImpl[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

}



class GreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with GreedyRandom[ModelID, ModelData, ModelAction]

