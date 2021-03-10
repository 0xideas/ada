package ada.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._

abstract class GreedyEnsembleAbstract2[ModelID, ModelData, ModelAction, AggregateReward <: ConditionalDistribution[ModelData]]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with StackableActor2[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImpl2[AggregateReward](models, modelRewards, epsilon, data, selectedIds)

}

class GreedyEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ConditionalDistribution[ModelData]]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyEnsembleAbstract2[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards, epsilon)
    with GreedyRandom[ModelID, ModelData, ModelAction]


    