package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class GreedySoftmaxDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with GreedySoftmax[ModelID, ModelData, ModelAction]
    with ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) =
    	_actImplD[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds)

    def update(modelId: ModelID, reward: Reward, data: ModelData): Unit = 
        modelRewards(modelId).update(data, reward)

    def export = export(models, modelKeys, modelRewards)

    override def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
        update(modelIds.head, reward, data)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }

}


class GreedySoftmaxWithContext
	[ModelID, Context, ModelData, ModelAction,
	 AggregateReward <: ContextualDistribution[Context]]
    (models: ModelID  => Model[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends ContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with GreedySoftmax[ModelID, ModelData, ModelAction]
    with ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{

    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID) = {
        _actImpl[Context, AggregateReward](models, modelKeys, modelRewards, epsilon, data, context)
    }

    def update(modelId: ModelID, context: Context, reward: Reward): Unit = 
        modelRewards(modelId).update(context, reward)

    def export: Json = export(models, modelKeys, modelRewards)
}
