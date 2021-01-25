package ada.core.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class Exp3Ensemble[ModelID, ModelData, ModelAction, AggregateReward <: Exp3Reward]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    gamma: Double)
    extends StackableEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with Exp3[ModelID, ModelData, ModelAction]{

    private var k: Int = modelKeys().length

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
        _actImpl[AggregateReward](models, modelKeys, modelRewards , 1.0, data, selectedIds, gamma, k)
    }

    override def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
                                            //this variable comes from the Exp3 Actor Trait
        val probability = (1.0-gamma)*reward.value/totalReward.value + gamma/k
        modelRewards(modelIds(0)).update(new Reward(reward.value/probability))
        models(modelIds.head).update(modelIds.tail, data, reward)
    }
}




