package epsilon.core.components.learners

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.distributions._



trait AbstractGreedy[ModelID, ModelData, ModelAction, AggregateReward]
	extends SelectModel[ModelID, ModelData, ModelAction, AggregateReward] {
    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                draw: AggregateReward => Double,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel(models, modelRewards, draw)
        
        if(rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelsSorted.tail, data)
    }
    def _actImpl[Context]
    			(models: Map[ModelID, Model[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                context: Context,
                draw: (Context, AggregateReward) => Double,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel[Context](models, modelRewards, context, draw)
        
        if(rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelsSorted.tail, data)
    }
}

trait GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward] 
    extends AbstractGreedy[ModelID, ModelData, ModelAction, AggregateReward]
    with ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]


//not used so far
trait Softmax[ModelID, ModelData, ModelAction, AggregateReward]
    extends ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]{
    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]],
                 modelRewards: ModelID => AggregateReward,
                 draw: AggregateReward => Double,
                 data: ModelData): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel(models, modelRewards, draw)
        _selectModel(models, modelsSorted, data)
    }
    def _actImpl[Context](models: Map[ModelID, Model[ModelData, ModelAction]],
                 modelRewards: ModelID => AggregateReward,
                 context: Context,
                 draw: (Context, AggregateReward) => Double,
                 data: ModelData): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel[Context](models, modelRewards, context, draw)
        _selectModel(models, modelsSorted, data)
    }
}


trait EpsilonEnsembleThompsonSampling
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
    extends PassiveEnsemble[ModelID, ModelData, ModelAction, Distr] {

    def draw: Distr => Double = (distr:Distr) => distr.draw

    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]],
                data: ModelData,
                modelRewards: ModelID => Distr): (ModelAction, ModelID) = {

        val modelsSorted = models.map{case(modelId,_) => {
                                        val reward = draw(modelRewards(modelId))
                                        (modelId, reward)
                                    }} 
                                    .toList
                                    .sortWith(_._2 > _._2)

        val selectedModelId = modelsSorted.head._1
        val selectedModel = models(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }     
}