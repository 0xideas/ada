package ada.core.components.selectors

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.core.interface._
import ada.core.components.distributions._

sealed trait Actor

trait CombinedActor[ModelID, ModelData, ModelAction]
    extends SimpleActor[ModelID, ModelData, ModelAction]
    with ContextualActor[ModelID, ModelData, ModelAction]
    with StackableActor[ModelID, ModelData, ModelAction]
    with StackableActor2[ModelID, ModelData, ModelAction]

trait SimpleActor[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) 
}

trait ContextualActor[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl[Context, AggregateReward <: ContextualDistribution[Context]]
    			(models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context): (ModelAction, ModelID) 
}

trait StackableActor[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) 
}

trait StackableActor2[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl2[AggregateReward <: ContextualDistribution[ModelData]]
                (models: ModelID => StackableModel2[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}

trait AbstractGreedy[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel[AggregateReward](modelKeys, modelRewards)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelKeys, modelsSorted.tail, data)    }

    def _actImpl[Context, AggregateReward <: ContextualDistribution[Context]]
    			(models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel[Context, AggregateReward](modelKeys, modelRewards, context)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelKeys, modelsSorted.tail, data)    }




    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {

        val modelsSorted = _sortModel[AggregateReward](modelKeys, modelRewards)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            selectedModel.actWithID(data, selectedIds ::: List(selectedModelId))
        }
        else {
            val (action, modelId) = _selectModel(models, modelKeys, modelsSorted.tail, data)
            (action, selectedIds ::: List(modelId))
        }
    }

    def _actImpl2[AggregateReward <: ContextualDistribution[ModelData]]
                (models: ModelID => StackableModel2[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
                                    //ModelData is also used as Context!!!
        val modelsSorted = _sortModel[ModelData, AggregateReward](modelKeys, modelRewards, data)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            selectedModel.actWithID(data, selectedIds ::: List(selectedModelId))
        }
        else {
            val (action, modelId) = _selectModel(models, modelKeys, modelsSorted.tail, data)
            (action, selectedIds ::: List(modelId))
        }
    }
}


trait Softmax[ModelID, ModelData, ModelAction]
    extends SoftmaxSelector[ModelID, ModelData, ModelAction]{

    def _actImpl[Context, AggregateReward <: ContextualDistribution[Context]](
                models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel[Context, AggregateReward](modelKeys, modelRewards, context)
        _selectModel(models, modelKeys, modelsSorted, data)
    }

    def _actImpl[AggregateReward <: SimpleDistribution](
                models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
        val modelsSorted = _sortModel[AggregateReward](modelKeys, modelRewards)
        val (action, modelId) = _selectModel(models, modelKeys, modelsSorted, data)
        (action, selectedIds ::: List(modelId) )
    }
}

trait Exp3[ModelID, ModelData, ModelAction]
    extends SoftmaxSelector[ModelID, ModelData, ModelAction]{

    protected var totalReward: Reward = 0.0

    def _adjustRewards(modelsSorted: List[(ModelID, Reward)], gamma: Double, k: Int): List[(ModelID, Reward)] = {
        totalReward = modelsSorted.map(_._2).sum
        modelsSorted.map{case(id, value) => (id, ((1.0 - gamma) * value/totalReward + gamma/k ))}
    }

    def _actImpl[Context, AggregateReward <: ContextualDistribution[Context]](
                models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                gamma: Double,
                k: Int): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel[Context, AggregateReward](modelKeys, modelRewards, context)
        _selectModel(models, modelKeys, _adjustRewards(modelsSorted, gamma, k), data)
    }

    def _actImpl[AggregateReward <: SimpleDistribution](
                models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID],
                gamma: Double,
                k: Int): (ModelAction, List[ModelID]) = {
        val modelsSorted = _sortModel[AggregateReward](modelKeys, modelRewards)
        val (action, modelId) = _selectModel(models, modelKeys, _adjustRewards(modelsSorted, gamma, k), data)
        (action, selectedIds ::: List(modelId) )
    }
}
