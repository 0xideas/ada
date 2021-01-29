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
                (models: ModelID => SimpleModel[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) 
}

trait ContextualActor[ModelID, Context, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with Actor{
    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]]
    			(models: ModelID => ContextualModel[ModelID, Context, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                modelIds: List[ModelID]): (ModelAction, List[ModelID]) 
}

trait StackableActor1[ModelID, ModelData, ModelAction]
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
    def _actImpl2[AggregateReward <: ConditionalDistribution[ModelData]]
                (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}

trait AbstractGreedy[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]
    with CombinedActor[ModelID, ModelData, ModelAction]{

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => SimpleModel[ModelData, ModelAction],
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
        else {
            val modelId = _selectModel(modelsSorted.tail) 
            (models(modelId).act(data), modelId)
        }
    }

    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]]
    			(models: ModelID => ContextualModel[ModelID, Context, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {

        val modelsSorted = _sortModel[Context, AggregateReward](modelKeys, modelRewards, context)

        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            selectedModel.actWithID(context, data, selectedIds++List(selectedModelId))
        }
        else {
            val selectedModelId = _selectModel(modelsSorted.tail) 
            models(selectedModelId).actWithID(context, data, selectedIds ++ List(selectedModelId))
        }
    }


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
            selectedModel.actWithID(data, selectedIds ++ List(selectedModelId))
        }
        else {
            val selectedModelId = _selectModel(modelsSorted.tail)
            models(selectedModelId).actWithID(data, selectedIds ++ List(selectedModelId))
        }
    }

    def _actImpl2[AggregateReward <: ConditionalDistribution[ModelData]]
                (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
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
            selectedModel.actWithID(data, selectedIds ++ List(selectedModelId))
        }
        else {
            val selectedModelId = _selectModel(modelsSorted.tail)
            models(selectedModelId).actWithID(data, selectedIds ++ List(selectedModelId))
        }
    }
}


trait Softmax[ModelID, ModelData, ModelAction]
    extends SoftmaxSelector[ModelID, ModelData, ModelAction]{

    def _actImpl[Context, AggregateReward <: ConditionalDistribution[Context]](
                models: ModelID => ContextualModel[ModelID, Context, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
        val modelsSorted = _sortModel[Context, AggregateReward](modelKeys, modelRewards, context)
        val modelId = _selectModel(modelsSorted)
        models(modelId).actWithID(context, data, selectedIds ++ List(modelId))
    }

    def _actImpl[AggregateReward <: SimpleDistribution](
                models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
        val modelsSorted = _sortModel[AggregateReward](modelKeys, modelRewards)
        val selectedModelId = _selectModel(modelsSorted)
        models(selectedModelId).actWithID(data, selectedIds ++ List(selectedModelId))
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
                models: ModelID => ContextualModel[ModelID, Context, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context,
                selectedIds: List[ModelID],
                gamma: Double,
                k: Int): (ModelAction, List[ModelID]) = {
        val modelsSorted = _sortModel[Context, AggregateReward](modelKeys, modelRewards, context)
        val selectedModelId = _selectModel(_adjustRewards(modelsSorted, gamma, k))
        models(selectedModelId).actWithID(context, data, selectedIds ++ List(selectedModelId))
    }

    def _actImpl[AggregateReward <: SimpleDistribution](
                models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID],
                gamma: Double,
                k: Int): (ModelAction, List[ModelID]) = {
        val modelsSorted = _sortModel[AggregateReward](modelKeys, modelRewards)
        val selectedModelId = _selectModel(modelsSorted)
        models(selectedModelId).actWithID(data, selectedIds ++ List(selectedModelId))
    }
}
