package epsilon
import scala.collection.mutable.{Map => MutableMap}

abstract class EpsilonEnsembleRoot[ModelData, ModelAction, ModelId](epsilon: Double, models: Map[ModelId, Model[ModelData, ModelAction]]) 
    extends EpsilonEnsembleInterface(epsilon, models) {


    private val idToModel: Map[ModelId, Model[ModelData, ModelAction]] = models
    private val modelToId: Map[Model[ModelData, ModelAction], ModelId] = models.map{ case(k, v) => (v, k)}
    private val modelIds: List[ModelId] = idToModel.keys.toList

    def getModelId(model: Model[ModelData, ModelAction]): ModelId = modelToId(model)
    def getModel(id: ModelId): Model[ModelData, ModelAction]  = idToModel(id)

    def act(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, correctAction: ModelAction): Reward
    def update(modelId: ModelId, action: ModelAction, correctAction: ModelAction): Unit = update(modelId, evaluate(action, correctAction))
    def update(modelId: ModelId, reward: Reward): Unit


    protected val rnd = new scala.util.Random
    def explore(modelsExplore: List[(ModelId, AggregateReward)], data: ModelData): (ModelAction, ModelId) = {
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
        else explore(modelsSorted.tail, data)
    }

    def learnRoot(data: ModelData,
                    correct: ModelAction,
                    which: AggregateReward => Boolean,
                    evaluation: (ModelAction, ModelAction) => Reward,
                    modelRewards: ModelId => AggregateReward): Unit = {
        modelIds.map(modelId => (modelId, modelRewards(modelId)))
                .filter{ case(modelId, reward) => which(reward)}
                .map{    case(modelId, reward) => (modelId, getModel(modelId)) }
                .map{    case(modelId, model) => {
                    val modelAction = model.act(data)
                    update(modelId, evaluation(correct, modelAction)) 
                }}
    }
}

trait EpsilonLearner[ModelData, ModelAction, ModelId]{
    def learn(data: ModelData,
            correct: ModelAction,
            which: AggregateReward => Boolean ): Unit
}

//reward must be positive
class EpsilonEnsemble[ModelData, ModelAction, ModelId](epsilon: Double,
                      models: Map[ModelId, Model[ModelData, ModelAction]],
                      updateFn: (ModelId, Reward) => Unit,
                      modelRewards: ModelId => AggregateReward,
                      evaluationFn: (ModelAction, ModelAction) => Reward ) extends EpsilonEnsembleRoot(epsilon, models) {

    def update(modelId: ModelId, reward: Reward): Unit = updateFn(modelId, reward)
    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, correctAction: ModelAction): Reward = evaluationFn(action, correctAction)
}


class EpsilonEnsembleLearner[ModelData, ModelAction, ModelId](epsilon: Double,
                      models: Map[ModelId, Model[ModelData, ModelAction]],
                      updateFn: (ModelId, Reward) => Unit,
                      modelRewards: ModelId => AggregateReward,
                      evaluationFn: (ModelAction, ModelAction) => Reward) extends EpsilonEnsemble(epsilon, models, updateFn, modelRewards, evaluationFn)
                                                                with EpsilonLearner[ModelData, ModelAction, ModelId]{

    def learn(data: ModelData,
              correct: ModelAction,
              which: AggregateReward => Boolean = aggReward => true): Unit = learnRoot(data, correct, which, evaluationFn, modelRewards)
}

class EpsilonEnsembleLocal[ModelData, ModelAction, ModelId](epsilon: Double,
                            models: Map[ModelId, Model[ModelData, ModelAction]],
                            newReward: (AggregateReward, Reward) => AggregateReward,
                            modelRewardsMap: MutableMap[ModelId, AggregateReward]) extends EpsilonEnsembleRoot(epsilon, models) {

    def getModelRewardsMap = modelRewardsMap

    val modelRewards = (modelId) => modelRewardsMap(modelId)

    def update(modelId: ModelId, reward: Reward): Unit = {
        modelRewardsMap(modelId) = newReward(modelRewards(modelId), reward)
    }
    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
}

object EpsilonEnsembleLocal {
    def apply[ModelData, ModelAction, ModelId](epsilon: Double,
                                      models: Map[ModelId, Model[ModelData, ModelAction]],
                                      newReward: (AggregateReward, Reward) => AggregateReward): EpsilonEnsembleLocal[ModelData, ModelAction, ModelId] = {
        val modelRewardsMap = MutableMap(models.keys.toList.zip(List.fill(models.size)(1.0)):_*)
        new EpsilonEnsembleLocal(epsilon, models, newReward, modelRewardsMap)
    }
    def apply[ModelData, ModelAction, ModelId](epsilon: Double,
                                      models: Map[ModelId, Model[ModelData, ModelAction]],
                                      newReward: (AggregateReward, Reward) => AggregateReward,
                                      modelRewardsMap: MutableMap[ModelId, AggregateReward]): EpsilonEnsembleLocal[ModelData, ModelAction, ModelId] = {
        new EpsilonEnsembleLocal(epsilon, models, newReward,  modelRewardsMap)
    }
}

