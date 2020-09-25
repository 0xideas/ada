package epsilon

import scala.collection.mutable.{Map => MutableMap}
import epsilon.{EpsilonEnsembleInterface, EpsilonLearner}

abstract class EpsilonEnsembleGreedySoftmax[ModelId, ModelData, ModelAction](epsilon: Double, models: Map[ModelId, Model[ModelData, ModelAction]]) 
    extends EpsilonEnsembleInterface(epsilon, models) {

    private val idToModel: Map[ModelId, Model[ModelData, ModelAction]] = models
    private val modelToId: Map[Model[ModelData, ModelAction], ModelId] = models.map{ case(k, v) => (v, k)}
    protected val modelIds: List[ModelId] = idToModel.keys.toList

    def getModelId(model: Model[ModelData, ModelAction]): ModelId = modelToId(model)
    def getModel(id: ModelId): Model[ModelData, ModelAction]  = idToModel(id)

    def act(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelId, reward: Reward): Unit


    protected val rnd = new scala.util.Random
    def exploreSoftmax(modelsExplore: List[(ModelId, AggregateReward)], data: ModelData): (ModelAction, ModelId) = {
        val totalReward: AggregateReward = modelsExplore.foldLeft(0.0)((agg, tup) => agg + tup._2)
        printEpsilon(totalReward.toString)
        printEpsilon(modelsExplore.toString())
        val cumulativeProb: List[(Probability, Probability)] = modelsExplore.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        //Softmax
        val modelsCumulativeProb: List[(ModelId, (Probability, Probability))] = modelsExplore.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        //select by relative probability
        val selectedModelId: ModelId = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        val selectedModel: Model[ModelData, ModelAction] = getModel(selectedModelId)
        printEpsilon("exploring... " + selectedModel.toString)
        (selectedModel.act(data), selectedModelId)
    }

    def actRoot(data: ModelData, modelRewards: ModelId => AggregateReward): (ModelAction, ModelId) = {
        val modelsSorted = modelIds.map(modelId => (modelId, modelRewards(modelId)))
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
class EpsilonEnsembleGreedySoftmaxGeneral[ModelId, ModelData, ModelAction](epsilon: Double,
                      models: Map[ModelId, Model[ModelData, ModelAction]],
                      updateFn: (ModelId, Reward) => Unit,
                      modelRewards: ModelId => AggregateReward,
                      evaluationFn: (ModelAction, ModelAction) => Reward ) extends EpsilonEnsembleGreedySoftmax(epsilon, models) {

    def update(modelId: ModelId, reward: Reward): Unit = updateFn(modelId, reward)

    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
}


class EpsilonEnsembleGreedySoftmaxLocal[ModelData, ModelAction, ModelId](epsilon: Double,
                            models: Map[ModelId, Model[ModelData, ModelAction]],
                            newReward: (AggregateReward, Reward) => AggregateReward,
                            evaluationFn: (ModelAction, ModelAction) => Reward,
                            modelRewardsMap: MutableMap[ModelId, AggregateReward]) extends EpsilonEnsembleGreedySoftmax(epsilon, models)
                                                                                   with EpsilonLearner[ModelId, ModelData, ModelAction]{

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
    def apply[ModelId, ModelData, ModelAction](epsilon: Double,
                                      models: Map[ModelId, Model[ModelData, ModelAction]],
                                      newReward: (AggregateReward, Reward) => AggregateReward,
                                      evaluationFn: (ModelAction, ModelAction) => Reward): EpsilonEnsembleGreedySoftmaxLocal[ModelData, ModelAction, ModelId] = {
        val modelRewardsMap = MutableMap(models.keys.toList.zip(List.fill(models.size)(1.0)):_*)
        new EpsilonEnsembleGreedySoftmaxLocal(epsilon, models, newReward, evaluationFn, modelRewardsMap)
    }
    def apply[ModelId, ModelData, ModelAction](epsilon: Double,
                                      models: Map[ModelId, Model[ModelData, ModelAction]],
                                      newReward: (AggregateReward, Reward) => AggregateReward,
                                      evaluationFn: (ModelAction, ModelAction) => Reward,
                                      modelRewardsMap: MutableMap[ModelId, AggregateReward]): EpsilonEnsembleGreedySoftmaxLocal[ModelData, ModelAction, ModelId] = {
        new EpsilonEnsembleGreedySoftmaxLocal(epsilon, models, newReward, evaluationFn, modelRewardsMap)
    }
}

