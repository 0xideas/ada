package ada.components.selectors

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.interface._
import ada.components.distributions._


sealed trait Actor

trait CombinedActor[ModelID, ModelData, ModelAction]
    extends SimpleActor[ModelID, ModelData, ModelAction]
    with StackableActor1[ModelID, ModelData, ModelAction]
    with StackableActor2[ModelID, ModelData, ModelAction]


trait SimpleActor[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: Map[ModelID, SimpleModel[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData): ( Tree[ModelAction], ModelID) 
}


trait ContextualActor[ModelID, Context, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]]
    			(models: Map[ModelID, ContextualModel[ModelID, Context, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                modelIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) 
}


trait StackableActor1[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) 
}


trait StackableActor2[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl2[AggregateReward <: ConditionalDistribution[ModelData]]
                (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID])
}


trait AbstractGreedy[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with CombinedActor[ModelID, ModelData, ModelAction]{

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: Map[ModelID, SimpleModel[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData): ( Tree[ModelAction], ModelID) = {

        val modelsSorted = _sortModel[AggregateReward](models.keys.toList, modelRewards)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else {
            val modelId = _selectModel(modelsSorted.tail) 
            (models(modelId).act(data), modelId)
        }
    }

    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]]
    			(models: Map[ModelID, ContextualModel[ModelID, Context, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) = {

        val modelsSorted = _sortModel[Context, AggregateReward](models.keys.toList, modelRewards, context)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            selectedModel.actWithID(context, data, appendId(selectedModelId, selectedIds))
        }
        else {
            val selectedModelId = _selectModel(modelsSorted.tail) 
            models(selectedModelId).actWithID(context, data, appendId(selectedModelId, selectedIds))
        }
    }

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) = {

        val modelsSorted = _sortModel[AggregateReward](models.keys.toList, modelRewards)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            selectedModel.actWithID(data,  appendId(selectedModelId, selectedIds))
        }
        else {
            val selectedModelId = _selectModel(modelsSorted.tail)
            models(selectedModelId).actWithID(data, appendId(selectedModelId, selectedIds))
        }
    }

    def _actImpl2[AggregateReward <: ConditionalDistribution[ModelData]]
                (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) = {
                                    //ModelData is also used as Context!!!
        val modelsSorted = _sortModel[ModelData, AggregateReward](models.keys.toList, modelRewards, data)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            selectedModel.actWithID(data,  appendId(selectedModelId, selectedIds))
        }
        else {
            val selectedModelId = _selectModel(modelsSorted.tail)
            models(selectedModelId).actWithID(data,  appendId(selectedModelId, selectedIds))
        }
    }
}


trait Softmax[ModelID, ModelData, ModelAction]
    extends SoftmaxSelector[ModelID, ModelData, ModelAction]{

    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]](
                models: Map[ModelID, ContextualModel[ModelID, Context, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) = {
        val modelsSorted = _sortModel[Context, AggregateReward](models.keys.toList, modelRewards, context)
        val selectedModelId = _selectModel(modelsSorted)
        models(selectedModelId).actWithID(context, data,  appendId(selectedModelId, selectedIds))
    }

    def _actImpl[AggregateReward <: SimpleDistribution](
                models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID]) = {
        val modelsSorted = _sortModel[AggregateReward](models.keys.toList, modelRewards)
        val selectedModelId = _selectModel(modelsSorted)
        models(selectedModelId).actWithID(data, appendId(selectedModelId, selectedIds))
    }
}


trait Exp3[ModelID, ModelData, ModelAction]
    extends SoftmaxSelector[ModelID, ModelData, ModelAction]{

    protected var totalReward = new Reward(0.0)

    def _adjustRewards(modelsSorted: List[(ModelID, Reward)], gamma: Double, k: Int): List[(ModelID, Reward)] = {
        totalReward = new Reward(modelsSorted.map(_._2.value).sum + modelsSorted.length*gamma/k)
        modelsSorted.map{case(id, reward) => (id, new Reward((1.0 - gamma) * reward.value/totalReward.value + gamma/k ))}
    }

    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]](
                models: Map[ModelID, ContextualModel[ModelID, Context, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                selectedIds: Tree[ModelID],
                gamma: Double,
                k: Int): ( Tree[ModelAction], Tree[ModelID]) = {
        val modelsSorted = _sortModel[Context, AggregateReward](models.keys.toList, modelRewards, context)
        val selectedModelId = _selectModel(_adjustRewards(modelsSorted, gamma, k))
        models(selectedModelId).actWithID(context, data,  appendId(selectedModelId, selectedIds))
    }

    def _actImpl[AggregateReward <: SimpleDistribution](
                models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: Tree[ModelID],
                gamma: Double,
                k: Int): ( Tree[ModelAction], Tree[ModelID]) = {
        val modelsSorted = _sortModel[AggregateReward](models.keys.toList, modelRewards)
        val selectedModelId = _selectModel(modelsSorted)
        models(selectedModelId).actWithID(data,  appendId(selectedModelId, selectedIds))
    }
}
