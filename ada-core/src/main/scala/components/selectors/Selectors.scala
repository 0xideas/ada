package ada.core.components.selectors

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.core.interface._
import ada.core.components.distributions._


trait Selector[ModelID, ModelData, ModelAction]{
    protected val rnd = new scala.util.Random(101)

    def _sortModel[AggregateReward <: SimpleDistribution](
                 modelKeys: () => List[ModelID],
                 modelRewards: ModelID => AggregateReward): List[(ModelID, Reward)] = {
        val modelIds = modelKeys()
        val modelsSorted = modelIds.map(modelId => (modelId, modelRewards(modelId).draw))
                                        .toList
                                        .sortWith(_._2 > _._2)
        modelsSorted
    }

    def _sortModel[Context, AggregateReward <: ContextualDistribution[Context]]
    			  (modelKeys: () => List[ModelID],
                   modelRewards: ModelID => AggregateReward,
                   context: Context): List[(ModelID, Reward)] = {
        val modelIds = modelKeys()
        val modelsSorted = modelIds.map(modelId => (modelId, modelRewards(modelId).draw(context)))
                                        .toList
                                        .sortWith(_._2 > _._2)
        modelsSorted
    }

    def _selectModel(modelKeys: () => List[ModelID],
            aggregateRewardsDouble: List[(ModelID, Reward)]): ModelID
    
}



trait SoftmaxSelector[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]{

    def _selectModel(modelKeys: () => List[ModelID],
                     aggregateRewardsDouble: List[(ModelID, Reward)]):  ModelID = {
        val totalReward: Reward = aggregateRewardsDouble.foldLeft(0.0)((agg, tup) => agg + tup._2)
        val cumulativeProb: List[(Probability, Probability)] = 
        	aggregateRewardsDouble
        		.scanLeft((0.0, 0.0))((acc, item) => (acc._2, acc._2 + item._2/totalReward)).tail

        val modelsCumulativeProb: List[(ModelID, (Probability, Probability))] = 
        	aggregateRewardsDouble.map(_._1).zip(cumulativeProb)

        val selector = rnd.nextDouble()
        val selectedModelId: ModelID = 
        	modelsCumulativeProb.filter{case(model, bounds) => 
        								(selector >= bounds._1) && (selector <= bounds._2)}(0)._1


        selectedModelId
    }
}

trait RandomSelector[ModelID, ModelData, ModelAction]
    extends Selector[ModelID, ModelData, ModelAction]{

    def _selectModel(modelKeys: () => List[ModelID],
                     aggregateRewardsDouble: List[(ModelID, Reward)]): ModelID = {
        val modelKeysV = modelKeys()
        val selector = rnd.nextInt(modelKeysV.length)
        val selectedModelId = modelKeysV(selector)
        selectedModelId
    }
}

