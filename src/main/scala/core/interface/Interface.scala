package epsilon.core.interface

import scala.collection.mutable.{Map => MutableMap}

import epsilon._
import epsilon.core.components.distributions.ContextualDistribution


abstract class EpsilonEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends Model[ModelData, ModelAction]{
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def models(): Map[ModelID, Model[ModelData, ModelAction]] = models
    def modelRewards(): MutableMap[ModelID, AggregateReward] = modelRewards
    def modelRewards(id: ModelID):  AggregateReward = modelRewards()(id)

}

abstract class EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends EpsilonEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelRewards)
    with ModelNoContext[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def act(data: ModelData): ModelAction = actWithID(data)._1
    def update(modelId: ModelID, reward: Reward): Unit
    def update(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, evaluate(action, optimalAction))

}

abstract class EpsilonEnsembleWithContext[ModelID, Context, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends EpsilonEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelRewards)
    with ModelWithContext[Context, ModelData, ModelAction]{
    def update(modelId: ModelID, context: Context, reward: Reward): Unit
    def update(modelId: ModelID, context: Context, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, context, evaluate(action, optimalAction))
    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID)
    def act(context: Context, data: ModelData): ModelAction = actWithID(context, data)._1
    //override def act[Context](context: Context, data: ModelData): ModelAction = actWithID(context, data)._1
}
