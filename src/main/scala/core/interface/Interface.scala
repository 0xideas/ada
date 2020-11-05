package epsilon.core.interface

import scala.collection.mutable.{Map => MutableMap}

import epsilon._
import epsilon.core.components.distributions.ContextualDistribution


trait EpsilonEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    extends Model[ModelData, ModelAction]{
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
}

trait EpsilonEnsembleWithContext[ModelID, Context, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsemble[ModelID,  ModelData, ModelAction, AggregateReward]
    with ModelWithContext[Context, ModelData, ModelAction]{
    def update(modelId: ModelID, context: Context, reward: Reward): Unit
    def update(modelId: ModelID, context: Context, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, context, evaluate(action, optimalAction))
    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID)
    def act(context: Context, data: ModelData): ModelAction = actWithID(context, data)._1
    //override def act[Context](context: Context, data: ModelData): ModelAction = actWithID(context, data)._1

}

trait EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsemble[ModelID,  ModelData, ModelAction, AggregateReward]
    with ModelNoContext[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def act(data: ModelData): ModelAction = actWithID(data)._1
    def update(modelId: ModelID, reward: Reward): Unit
    def update(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, evaluate(action, optimalAction))

}
