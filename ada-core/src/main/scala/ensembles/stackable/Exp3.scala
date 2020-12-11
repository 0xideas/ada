package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._

/*
abstract class Exp3[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with Softmax[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImpl[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

    def update(modelId: ModelID, reward: Reward): Unit = 
        modelRewards(modelId).update(reward)

    override def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
        update(modelIds.head, reward)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }

}*/