package ada.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import io.circe.Decoder

abstract class GreedyEnsembleAbstract2[ModelID, ModelData, ModelAction, AggregateReward <: ConditionalDistribution[ModelData]]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)(implicit modelIdDecoder: Decoder[ModelID])
    extends StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with StackableActor2[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImpl2[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

}

class GreedyEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ConditionalDistribution[ModelData]]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)(implicit modelIdDecoder: Decoder[ModelID])
    extends GreedyEnsembleAbstract2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with GreedyRandom[ModelID, ModelData, ModelAction]


    