package ada.core.interface

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.core.components.distributions.ContextualDistribution
import org.apache.logging.log4j.core.appender.rewrite.MapRewritePolicy.Mode


abstract class AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends Model[ModelData, ModelAction]{

    //to passive attribute
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward

    def models(): Map[ModelID, Model[ModelData, ModelAction]] = models
    def modelRewards(): MutableMap[ModelID, AggregateReward] = modelRewards
    def modelRewards(id: ModelID):  AggregateReward = modelRewards()(id)

}

abstract class SimpleEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelRewards)
    with SimpleModel[ModelData, ModelAction]{
    def actWithID(data: ModelData): (ModelAction, ModelID)
    def act(data: ModelData): ModelAction = actWithID(data)._1
    def update(modelId: ModelID, reward: Reward): Unit

    //to passive attribute
    def updateSingle(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, evaluate(action, optimalAction))

}

abstract class ContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelRewards)
    with ContextualModel[Context, ModelData, ModelAction]{
    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID)
    def act(context: Context, data: ModelData): ModelAction = actWithID(context, data)._1
    def update(modelId: ModelID, context: Context, reward: Reward): Unit

    //to passive attribute
    def updateSingle(modelId: ModelID, context: Context, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, context, evaluate(action, optimalAction))
    //override def act[Context](context: Context, data: ModelData): ModelAction = actWithID(context, data)._1
}


abstract class StackableEnsemble[ModelID, ModelData, ModelAction, AggregateReward](
    models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with StackableModel[ModelID, ModelData, ModelAction]{
        def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
        def act(data: ModelData, selectedIds: List[ModelID]): ModelAction = actWithID(data, selectedIds)._1
        def act(data: ModelData): ModelAction = actWithID(data, List())._1
        def update(modelId: ModelID, reward: Reward): Unit

    //to passive attribute
    def updateSingle(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = 
        update(modelId, evaluate(action, optimalAction))
}

/*
abstract class StackableSimpleEnsemble[ModelID, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[Unit, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends SimpleEnsemble[ModelID, Unit, ModelAction, AggregateReward](models, modelRewards)

abstract class StackableContextualEnsemble[ModelID, Context, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[Context, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward])
    extends ContextualEnsemble[ModelID, Context, Context, ModelAction, AggregateReward](models, modelRewards)


trait SingleType[A]
trait IdenticalTypeParameters1[A, B <: A]
trait IdenticalTypeParameters2[A, B <: A]

    trait Stackable[Context, ModelData]
    extends IdenticalTypeParameters1[Context, ModelData]
    with IdenticalTypeParameters2[ModelData, Context]*/