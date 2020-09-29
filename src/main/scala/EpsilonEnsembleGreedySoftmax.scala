package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}
import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonLearner, Model}

import epsilon._

abstract class EpsilonEnsembleGreedySoftmax[ModelId, ModelData, ModelAction, AggregateReward](epsilon: Double,
    models: Map[ModelId, Model[ModelData, ModelAction]], draw: AggregateReward => Double) 
    extends EpsilonEnsembleInterface(epsilon, models, draw) {

    private val idToModel: Map[ModelId, Model[ModelData, ModelAction]] = models
    private val modelToId: Map[Model[ModelData, ModelAction], ModelId] = models.map{ case(k, v) => (v, k)}
    protected val modelIds: List[ModelId] = idToModel.keys.toList

    def getModelId(model: Model[ModelData, ModelAction]): ModelId = modelToId(model)
    def getModel(id: ModelId): Model[ModelData, ModelAction]  = idToModel(id)

    def act(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelId, reward: Reward): Unit


    protected val rnd = new scala.util.Random
    def exploreSoftmax(aggregateRewardsDouble: List[(ModelId, Double)], data: ModelData): (ModelAction, ModelId) = {
        val totalReward: Double = aggregateRewardsDouble.foldLeft(0.0)((agg, tup) => agg + tup._2)
        printEpsilon(totalReward.toString)
        val cumulativeProb: List[(Probability, Probability)] = aggregateRewardsDouble.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        //Softmax
        val modelsCumulativeProb: List[(ModelId, (Probability, Probability))] = aggregateRewardsDouble.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        //select by relative probability
        val selectedModelId: ModelId = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        val selectedModel: Model[ModelData, ModelAction] = getModel(selectedModelId)
        printEpsilon("exploring... " + selectedModel.toString)
        (selectedModel.act(data), selectedModelId)
    }

    def actRoot(data: ModelData, modelRewards: ModelId => AggregateReward): (ModelAction, ModelId) = {
        val modelsSorted = modelIds.map(modelId => (modelId, draw(modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)

        if(rnd.nextDouble() > epsilon) {
            printEpsilon("exploiting...")
            val selectedModelId = modelsSorted.head._1
            val selectedModel = getModel(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else exploreSoftmax(modelsSorted.tail, data)
    }

}



//reward must be positive
class EpsilonEnsembleGreedySoftmaxGeneral[ModelId, ModelData, ModelAction, AggregateReward](epsilon: Double,
                      models: Map[ModelId, Model[ModelData, ModelAction]],
                      updateFn: (ModelId, Reward) => Unit,
                      modelRewards: ModelId => AggregateReward,
                      draw: AggregateReward => Double,
                      evaluationFn: (ModelAction, ModelAction) => Reward ) extends EpsilonEnsembleGreedySoftmax(epsilon, models, draw) {

    def update(modelId: ModelId, reward: Reward): Unit = updateFn(modelId, reward)

    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
}


class EpsilonEnsembleGreedySoftmaxLocal[ModelData, ModelAction, ModelId, AggregateReward]
    (epsilon: Double,
     models: Map[ModelId, Model[ModelData, ModelAction]],
     newReward: (AggregateReward, Reward) => AggregateReward,
     evaluationFn: (ModelAction, ModelAction) => Reward,
     draw: AggregateReward => Double,
     modelRewardsMap: MutableMap[ModelId, AggregateReward])
     extends EpsilonEnsembleGreedySoftmax[ModelId, ModelData, ModelAction, AggregateReward](epsilon, models, draw)
     with EpsilonLearner[ModelId, ModelData, ModelAction, AggregateReward]{

    def getModelRewardsMap = modelRewardsMap
    val modelRewards = (modelId) => modelRewardsMap(modelId)


    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelId, reward: Reward): Unit = {
        val oldReward = modelRewards(modelId)
        val newRewardV =  newReward(oldReward, reward)
        printEpsilon(f"$modelId: $oldReward + $reward => $newRewardV")
        modelRewardsMap(modelId) = newRewardV
    }

    def learn(data: ModelData,
              correct: ModelAction,
              which: AggregateReward => Boolean = aggReward => true): Unit = learnRoot(modelIds, data, correct, which, evaluationFn, modelRewards)
}

object EpsilonEnsembleGreedySoftmaxLocal {
    def apply[ModelId, ModelData, ModelAction, AggregateReward](epsilon: Double,
                                      models: Map[ModelId, Model[ModelData, ModelAction]],
                                      newReward: (AggregateReward, Reward) => AggregateReward,
                                      evaluationFn: (ModelAction, ModelAction) => Reward,
                                      draw: AggregateReward => Double,
                                      initAggregateReward: AggregateReward): EpsilonEnsembleGreedySoftmaxLocal[ModelData, ModelAction, ModelId,  AggregateReward] = {
        val modelRewardsMap = MutableMap(models.keys.toList.zip(List.fill(models.size)(initAggregateReward)):_*)
        new EpsilonEnsembleGreedySoftmaxLocal(epsilon, models, newReward, evaluationFn, draw, modelRewardsMap)
    }
    def apply[ModelId, ModelData, ModelAction, AggregateReward](epsilon: Double,
                                      models: Map[ModelId, Model[ModelData, ModelAction]],
                                      newReward: (AggregateReward, Reward) => AggregateReward,
                                      evaluationFn: (ModelAction, ModelAction) => Reward,
                                      draw: AggregateReward => Double,
                                      modelRewardsMap: MutableMap[ModelId, AggregateReward]): EpsilonEnsembleGreedySoftmaxLocal[ModelData, ModelAction, ModelId, AggregateReward] = {
        new EpsilonEnsembleGreedySoftmaxLocal(epsilon, models, newReward, evaluationFn, draw, modelRewardsMap)
    }
}

