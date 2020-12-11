package ada.core.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._

abstract class GreedyDynamicEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModel2[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with StackableActor2[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImpl2[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
        modelRewards(modelIds(0)).update(data, reward)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }
}

class GreedyDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModel2[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyDynamicEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with Greedy[ModelID, ModelData, ModelAction]

