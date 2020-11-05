package epsilon.core.components.learners

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.distributions._


trait SelectModel[ModelID, ModelData, ModelAction, AggregateReward]{
    protected val rnd = new scala.util.Random(101)

    def _sortModel(models: Map[ModelID, Model[ModelData, ModelAction]],
                 modelRewards: ModelID => AggregateReward,
                 draw: AggregateReward => Double): List[(ModelID, Double)] = {
        val modelIds = models.keys
        val modelsSorted = modelIds.map(modelId => (modelId, draw(modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)
        modelsSorted
    }

    def _sortModel[Context]
    			  (models: Map[ModelID, Model[ModelData, ModelAction]],
                   modelRewards: ModelID => AggregateReward,
                   context: Context,
                   draw: (Context, AggregateReward) => Double): List[(ModelID, Double)] = {
        val modelIds = models.keys
        val modelsSorted = modelIds.map(modelId => (modelId, draw(context, modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)
        modelsSorted
    }

    def _selectModel(models: Map[ModelID, Model[ModelData, ModelAction]],
            aggregateRewardsDouble: List[(ModelID, Double)],
            data: ModelData): (ModelAction, ModelID)
}


trait ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    extends SelectModel[ModelID, ModelData, ModelAction, AggregateReward]{

    def _selectModel(models: Map[ModelID, Model[ModelData, ModelAction]],
                     aggregateRewardsDouble: List[(ModelID, Double)],
                     data: ModelData): (ModelAction, ModelID) = {
        val totalReward: Double = aggregateRewardsDouble.foldLeft(0.0)((agg, tup) => agg + tup._2)
        val cumulativeProb: List[(Probability, Probability)] = 
        	aggregateRewardsDouble
        		.scanLeft((0.0, 0.0))((acc, item) => (acc._2, acc._2 + item._2/totalReward)).tail

        val modelsCumulativeProb: List[(ModelID, (Probability, Probability))] = 
        	aggregateRewardsDouble.map(_._1).zip(cumulativeProb)

        val selector = rnd.nextDouble()
        val selectedModelId: ModelID = 
        	modelsCumulativeProb.filter{case(model, bounds) => 
        								(selector >= bounds._1) && (selector <= bounds._2)}(0)._1

        val selectedModel: Model[ModelData, ModelAction] = models(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }
}