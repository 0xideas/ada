package ada.core.interface


import ada._
import ada.core.components.distributions.ContextualDistribution
import org.apache.logging.log4j.core.appender.rewrite.MapRewritePolicy.Mode


abstract class AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: Exportable]
    (models: ModelID  => Model[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends Model[ModelData, ModelAction]
    with ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{

    def models(): Map[ModelID, Model[ModelData, ModelAction]] = models
    def modelRewards(): Map[ModelID, AggregateReward] = modelRewards
    def modelRewards(id: ModelID):  AggregateReward = modelRewards()(id)
    def export = export(models, modelKeys, modelRewards)
    def update(data: ModelData, reward: Reward): Unit = ()


}

abstract class SimpleEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]
    (models: ModelID  => SimpleModel[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with SimpleModel[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def act(data: ModelData): ModelAction = actWithID(data)._1
    def update(modelId: ModelID, data: ModelData, reward: Reward): Unit


}

abstract class ContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[Context]]
    (models: ModelID  => SimpleModel[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with ContextualModel[Context, ModelData, ModelAction]{
    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID)
    def act(context: Context, data: ModelData): ModelAction = actWithID(context, data)._1
    def update(modelId: ModelID, context: Context, data: ModelData, reward: Reward): Unit = {

        models(modelId).update(data, reward)
        modelRewards(modelId).update(context, reward)
    }
    def update(context: Context, data: ModelData, reward: Reward): Unit = ()
}


abstract class StackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable](
    models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
    modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with StackableModel[ModelID, ModelData, ModelAction]{
        def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
        def act(data: ModelData, selectedIds: List[ModelID]): ModelAction = actWithID(data, selectedIds)._1
        def act(data: ModelData): ModelAction = actWithID(data, List())._1
        def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
            modelRewards(modelIds(0)).update(reward)
            models(modelIds.head).update(modelIds.tail, data, reward)
        }

}

abstract class StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[ModelData]](
    models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
    modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with StackableModel[ModelID, ModelData, ModelAction]{
        def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
        def act(data: ModelData, selectedIds: List[ModelID]): ModelAction = actWithID(data, selectedIds)._1
        def act(data: ModelData): ModelAction = actWithID(data, List())._1
        def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = {
            modelRewards(modelIds(0)).update(data, reward)
            models(modelIds.head).update(modelIds.tail, data, reward)
        }
}

