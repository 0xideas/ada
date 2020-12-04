package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class GreedySoftmaxEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with GreedySoftmax[ModelID, ModelData, ModelAction]
    with ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImpl[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

    def update(modelId: ModelID, reward: Reward): Unit = 
        modelRewards(modelId).update(reward)

    def export = export(models, modelKeys, modelRewards)

    override def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
        update(modelIds.head, reward)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }

}


object GreedySoftmaxLocal{
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



