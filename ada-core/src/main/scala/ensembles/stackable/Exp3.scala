package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class Exp3[ModelID, ModelData, ModelAction, AggregateReward <: Exp3Reward]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    gamma: Double)
    extends StackableEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with Softmax[ModelID, ModelData, ModelAction]{

    private var totalRewardCache: Double = 0

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
        totalRewardCache = modelsSortedCache.map(_._2).sum
        _actImpl[AggregateReward](models, modelKeys, modelRewards, 1.0, data, selectedIds)
    }

    def update(modelIds: List[ModelID], reward: Reward): Unit = {
        val probability = reward/totalRewardCache
        modelRewards(modelIds(0)).update(reward/probability)
        models(modelIds.head).update(modelIds.tail, reward)
    }
}



