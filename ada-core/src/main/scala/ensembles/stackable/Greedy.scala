package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class GreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with Greedy[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImpl[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

    def update(modelId: ModelID, reward: Reward): Unit = 
        modelRewards(modelId).update(reward)


    override def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
        update(modelIds.head, reward)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }
}


object GreedyLocal{
    def apply[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution](
        models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
        initAggregateRewards: () => AggregateReward,
        epsilon: Double): GreedySoftmaxEnsemble[ModelID, ModelData, ModelAction, AggregateReward] = {
            val modelRewards = MutableMap(models.toSeq.map{case(k,v) => (k, initAggregateRewards())}:_*)
            val ensemble = new GreedySoftmaxEnsemble
                [ModelID, ModelData, ModelAction, AggregateReward](
                key => models(key), () => models.keys.toList, modelRewards, epsilon)
            ensemble
        }
    def apply[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution](
        models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
        modelRewards: MutableMap[ModelID, AggregateReward],
        epsilon: Double): GreedySoftmaxEnsemble[ModelID, ModelData, ModelAction, AggregateReward] = {
            val ensemble = new GreedySoftmaxEnsemble
                [ModelID, ModelData, ModelAction, AggregateReward](
                key => models(key), () => models.keys.toList, modelRewards, epsilon)
            ensemble
        }
}


class GreedyDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with Greedy[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImplD[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

    def update(modelId: ModelID, reward: Reward, data: ModelData): Unit = 
        modelRewards(modelId).update(data, reward)

    override def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
        update(modelIds.head, reward, data)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }
}
