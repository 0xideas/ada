package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}
import epsilon.interfaces.{EpsilonEnsemblePassive, Model, LocalEnsemble}

import epsilon._


trait GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]{
    protected val rnd = new scala.util.Random(101)

    def _exploreWithSoftmax(models: Map[ModelID, Model[ModelData, ModelAction]], aggregateRewardsDouble: List[(ModelID, Double)], data: ModelData): (ModelAction, ModelID) = {
        val totalReward: Double = aggregateRewardsDouble.foldLeft(0.0)((agg, tup) => agg + tup._2)
        val cumulativeProb: List[(Probability, Probability)] = aggregateRewardsDouble.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        val modelsCumulativeProb: List[(ModelID, (Probability, Probability))] = aggregateRewardsDouble.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        val selectedModelId: ModelID = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        val selectedModel: Model[ModelData, ModelAction] = models(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }

    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                draw: AggregateReward => Double,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelIds = models.keys
        val modelsSorted = modelIds.map(modelId => (modelId, draw(modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)
        if(rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _exploreWithSoftmax(models, modelsSorted.tail, data)
    }
}

class EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: AggregateReward => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward)
    extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, AggregateReward]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction, AggregateReward] {


    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(models, modelRewards, draw, epsilon, data)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)

    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(data, correct, models, modelRewards)
    
    def getModelRewards: MutableMap[ModelID, AggregateReward] = modelRewards

    def update(modelId: ModelID, reward: Reward): Unit = updateFn(modelId, reward, modelRewards,  updateAggregateRewardFn)

}
