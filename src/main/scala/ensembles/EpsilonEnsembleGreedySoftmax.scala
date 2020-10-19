package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}
import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonEnsemblePassive, Model}

import epsilon._

abstract class EpsilonEnsembleGreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward](epsilon: Double,
    models: Map[ModelID, Model[ModelData, ModelAction]], draw: AggregateReward => Double) 
    extends EpsilonEnsembleInterface(models, draw) {

    protected val modelIds: List[ModelID] = models.keys.toList

    protected val rnd = new scala.util.Random(101)
    def _exploreWithSoftmax(aggregateRewardsDouble: List[(ModelID, Double)], data: ModelData): (ModelAction, ModelID) = {
        val totalReward: Double = aggregateRewardsDouble.foldLeft(0.0)((agg, tup) => agg + tup._2)
        val cumulativeProb: List[(Probability, Probability)] = aggregateRewardsDouble.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        val modelsCumulativeProb: List[(ModelID, (Probability, Probability))] = aggregateRewardsDouble.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        val selectedModelId: ModelID = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        val selectedModel: Model[ModelData, ModelAction] = getModel(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }

    def _actImpl(data: ModelData, modelRewards: ModelID => AggregateReward): (ModelAction, ModelID) = {
        val modelsSorted = modelIds.map(modelId => (modelId, draw(modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)
        if(rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = getModel(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _exploreWithSoftmax(modelsSorted.tail, data)
    }

}


class EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward]
    (epsilon: Double,
     models: Map[ModelID, Model[ModelData, ModelAction]],
     updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward,
     evaluationFn: (ModelAction, ModelAction) => Reward,
     draw: AggregateReward => Double,
     modelRewardsMap: MutableMap[ModelID, AggregateReward])
     extends EpsilonEnsembleGreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward](epsilon, models, draw)
     with EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, AggregateReward]{

    def getModelRewardsMap = modelRewardsMap
    val modelRewards = (modelId) => modelRewardsMap(modelId)


    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(data, modelRewards)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelID, reward: Reward): Unit = {
        val oldReward = modelRewards(modelId)
        val newReward =  updateAggregateRewardFn(oldReward, reward)
        modelRewardsMap(modelId) = newReward
    }

    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(modelIds, data, correct, evaluationFn, modelRewards)
}

object EpsilonEnsembleGreedySoftmaxLocal {
    def apply[ModelID, ModelData, ModelAction, AggregateReward](epsilon: Double,
                                      models: Map[ModelID, Model[ModelData, ModelAction]],
                                      updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward,
                                      evaluationFn: (ModelAction, ModelAction) => Reward,
                                      draw: AggregateReward => Double,
                                      initAggregateReward: AggregateReward): EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward] = {
        val modelRewardsMap = MutableMap(models.keys.toList.zip(List.fill(models.size)(initAggregateReward)):_*)
        new EpsilonEnsembleGreedySoftmaxLocal(epsilon, models, updateAggregateRewardFn, evaluationFn, draw, modelRewardsMap)
    }
    def apply[ModelID, ModelData, ModelAction, AggregateReward](epsilon: Double,
                                      models: Map[ModelID, Model[ModelData, ModelAction]],
                                      updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward,
                                      evaluationFn: (ModelAction, ModelAction) => Reward,
                                      draw: AggregateReward => Double,
                                      modelRewardsMap: MutableMap[ModelID, AggregateReward]): EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward] = {
        new EpsilonEnsembleGreedySoftmaxLocal(epsilon, models, updateAggregateRewardFn, evaluationFn, draw, modelRewardsMap)
    }
}

//reward must be positive
class EpsilonEnsembleGreedySoftmaxGeneral[ModelID, ModelData, ModelAction, AggregateReward]
                    (epsilon: Double,
                      models: Map[ModelID, Model[ModelData, ModelAction]],
                      updateFn: (ModelID, Reward) => Unit,
                      evaluationFn: (ModelAction, ModelAction) => Reward,
                      draw: AggregateReward => Double,
                      modelRewardsFn: ModelID => AggregateReward) extends EpsilonEnsembleGreedySoftmax(epsilon, models, draw) {

    def update(modelId: ModelID, reward: Reward): Unit = updateFn(modelId, reward)

    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(data, modelRewardsFn)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
}