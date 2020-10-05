package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}
import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonLearner, Model}

import epsilon._


trait Distribution[Reward]{
    def draw: Double
    def update(reward:Reward): Unit
}

class BetaDistribution[Reward](alpha: Double, beta: Double) extends Distribution[Reward]{
    def draw = 0.0
    def update(reward:Reward) = ()
}

abstract class EpsilonEnsembleThompsonSampling[ModelId, ModelData, ModelAction, Distr <: Distribution[Reward] ](
    models: Map[ModelId, Model[ModelData, ModelAction]], draw: Distr => Double = (distr:Distr) => distr.draw) 
    extends EpsilonEnsembleInterface(models, draw) {

    private val idToModel: Map[ModelId, Model[ModelData, ModelAction]] = models
    private val modelToId: Map[Model[ModelData, ModelAction], ModelId] = models.map{ case(k, v) => (v, k)}
    protected val modelIds: List[ModelId] = idToModel.keys.toList

    def getModelId(model: Model[ModelData, ModelAction]): ModelId = modelToId(model)
    def getModel(id: ModelId): Model[ModelData, ModelAction]  = idToModel(id)

    def act(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelId, reward: Reward): Unit

    def actRoot(data: ModelData, modelRewards: ModelId => Distr): (ModelAction, ModelId) = {
        val modelsSorted = modelIds.map(modelId => (modelId, draw(modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)

        val selectedModelId = modelsSorted.head._1
        val selectedModel = getModel(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }

}

class EpsilonEnsembleThompsonSamplingGeneral[ModelId, ModelData, ModelAction, Distr <: Distribution[Reward]](
                      models: Map[ModelId, Model[ModelData, ModelAction]],
                      modelRewards: ModelId => Distr,
                      evaluationFn: (ModelAction, ModelAction) => Reward ) extends EpsilonEnsembleThompsonSampling(models) {

    def update(modelId: ModelId, reward: Reward): Unit =  {modelRewards(modelId).update(reward)}

    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
}


class EpsilonEnsembleThompsonSamplingLocal[ModelId, ModelData, ModelAction, Distr <: Distribution[Reward]]
    (models: Map[ModelId, Model[ModelData, ModelAction]],
     newReward: (Distr, Reward) => Distr,
     evaluationFn: (ModelAction, ModelAction) => Reward,
     modelRewardsMap: MutableMap[ModelId, Distr])
     extends EpsilonEnsembleThompsonSampling[ModelId, ModelData, ModelAction, Distr](models)
     with EpsilonLearner[ModelId, ModelData, ModelAction, Distr]{

    def getModelRewardsMap = modelRewardsMap
    val modelRewards = (modelId) => modelRewardsMap(modelId)

    def act(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelId, reward: Reward): Unit =  {modelRewardsMap(modelId).update(reward)}
    def learn(data: ModelData,
              correct: ModelAction,
              which: Distr => Boolean = aggReward => true): Unit = learnRoot(modelIds, data, correct, which, evaluationFn, modelRewards)
}
