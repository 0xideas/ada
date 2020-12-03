package ada.core.components.selectors

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.core.interface._
import ada.core.components.distributions._


trait AbstractGreedy[ModelID, ModelData, ModelAction]
	extends SelectModel[ModelID, ModelData, ModelAction] {
    def _actImplBase[AggregateReward <: Distribution]
                (models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                modelsSorted: List[(ModelID, Reward)]): (ModelAction, ModelID) = {
        if(epsilon == 0.0 || rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelKeys, modelsSorted.tail, data)
    }

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel[AggregateReward](models, modelKeys, modelRewards)
        _actImplBase[AggregateReward](models, modelKeys, modelRewards, epsilon, data, modelsSorted)
    }

    def _actImpl[Context, AggregateReward <: ContextualDistribution[Context]]
    			(models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                context: Context): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel[Context, AggregateReward](models, modelKeys, modelRewards, context)
        _actImplBase[AggregateReward](models, modelKeys, modelRewards, epsilon, data, modelsSorted)
    }


    def _actImplBaseStackable[AggregateReward <: Distribution]
        (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
        modelKeys: () => List[ModelID],
        modelRewards: ModelID => AggregateReward,
        epsilon: Double,
        data: ModelData,
        selectedIds: List[ModelID],
        modelsSorted: List[(ModelID, Reward)]): (ModelAction, List[ModelID]) = {

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

    def _actImpl[AggregateReward <: SimpleDistribution]
                (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {

        val modelsSorted = _sortModel[AggregateReward](models, modelKeys, modelRewards)
        _actImplBaseStackable[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds, modelsSorted)
    }

    def _actImplD[AggregateReward <: ContextualDistribution[ModelData]]
                (models: ModelID => StackableModel[ModelID, ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                modelRewards: ModelID => AggregateReward,
                epsilon: Double,
                data: ModelData,
                selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = {
                                    //ModelData is also used as Context!!!
        val modelsSorted = _sortModel[ModelData, AggregateReward](models, modelKeys, modelRewards, data)
        _actImplBaseStackable[AggregateReward](models, modelKeys, modelRewards, epsilon, data, selectedIds, modelsSorted)
    }
}


//not used so far
trait Softmax[ModelID, ModelData, ModelAction]
    extends SelectWithSoftmax[ModelID, ModelData, ModelAction]{
    def _actImpl[AggregateReward <: SimpleDistribution](models: ModelID => Model[ModelData, ModelAction],
                modelKeys: () => List[ModelID],
                 modelRewards: ModelID => AggregateReward,
                 data: ModelData): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel[AggregateReward](models, modelKeys, modelRewards)
        _selectModel(models, modelKeys, modelsSorted, data)
    }
    def _actImpl[Context, AggregateReward <: ContextualDistribution[Context]](models: ModelID => Model[ModelData, ModelAction],
                 modelKeys: () => List[ModelID],
                 modelRewards: ModelID => AggregateReward,
                 context: Context,
                 data: ModelData): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel[Context, AggregateReward](models, modelKeys, modelRewards, context)
        _selectModel(models, modelKeys, modelsSorted, data)
    }
}
