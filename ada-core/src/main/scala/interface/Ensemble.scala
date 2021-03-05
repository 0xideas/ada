package ada.interface


import ada._
import ada.components.distributions.ConditionalDistribution
import org.apache.logging.log4j.core.appender.rewrite.MapRewritePolicy.Mode
import io.circe.Json
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

abstract class AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    (models: ModelID  => Model[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends Model[ModelData, ModelAction]{

    def models(): Map[ModelID, Model[ModelData, ModelAction]] = modelKeys().map(id => (id, models(id))).toMap
    def modelRewards(): Map[ModelID, AggregateReward] = modelRewards
    def modelRewards(id: ModelID):  AggregateReward = modelRewards()(id)

}

abstract class SimpleEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]
    (models: ModelID  => SimpleModel[ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards){
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def act(data: ModelData): ModelAction = actWithID(data)._1
    def update(modelId: ModelID, reward: Reward): Unit = modelRewards(modelId).update(reward)
    def update(modelId: ModelID, data: ModelData, action: ModelAction): Unit = {
        models(modelId).update(data, action)
    }

}

abstract class ContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[Context]]
    (models: ModelID  => ContextualModel[ModelID, Context, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards)
    with ContextualModel[ModelID, Context, ModelData, ModelAction]{
    def actWithID(context: Context, data: ModelData, modelIds: List[ModelID]): (ModelAction, List[ModelID])
    def act(modelIds: List[ModelID], context: Context, data: ModelData): ModelAction = actWithID(context, data, modelIds)._1
    def update(modelIds: List[ModelID], context: Context, data: ModelData, reward: Reward): Unit = {
        modelRewards(modelIds.head).update(context, reward)
        models(modelIds.head).update(modelIds.tail, context, data, reward)
    }
    def update(modelIds: List[ModelID], context: Context, data: ModelData, action: ModelAction): Unit = {
        models(modelIds.head).update(modelIds.tail, context, data, action)
    }

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
        modelRewards(modelIds.head).update(reward)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }
    def update(modelIds: List[ModelID], data: ModelData, action: ModelAction): Unit = {
        models(modelIds.head).update(modelIds.tail, data, action)
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
        modelRewards(modelIds.head).update(data, reward)
        models(modelIds.head).update(modelIds.tail, data, reward)
    }
    def update(modelIds: List[ModelID], data: ModelData, action: ModelAction): Unit = {
        models(modelIds.head).update(modelIds.tail, data, action)
    }


}


