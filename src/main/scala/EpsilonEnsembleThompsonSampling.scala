package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.distributions.{Beta, Bernoulli}


import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonLearner, Model}
import epsilon._


trait Distribution[Reward <: Double]{
    def draw: Double
    def update(reward: Reward): Unit
}

class BetaDistribution[Reward <: Double](private var alpha: Double, private var beta: Double)
    extends Distribution[Reward]{
    private var betaDistribution = Beta(alpha, beta)
    override def toString: String = {
        f"alpha: $alpha beta: $beta"
    }

    def draw = betaDistribution.draw()

    def update(reward:Reward):Unit = {
        val rewardNormed = math.max(math.min(reward, 1), 0)
        alpha = alpha + rewardNormed
        beta = beta + (1.0-rewardNormed)
        betaDistribution = Beta(alpha, beta)
    }
}

abstract class EpsilonEnsembleThompsonSampling[ModelId, ModelData, ModelAction, Distr <: Distribution[Reward] ](
    models: Map[ModelId, Model[ModelData, ModelAction]], draw: Distr => Double = (distr:Distr) => distr.draw) 
    extends EpsilonEnsembleInterface(models, draw) {

    private val idToModel: Map[ModelId, Model[ModelData, ModelAction]] = models
    private val modelToId: Map[Model[ModelData, ModelAction], ModelId] = models.map{ case(k, v) => (v, k)}
    protected val modelIds: List[ModelId] = idToModel.keys.toList

    def getModelId(model: Model[ModelData, ModelAction]): ModelId = modelToId(model)
    def getModel(id: ModelId): Model[ModelData, ModelAction]  = idToModel(id)

    def actWithID(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelId, reward: Reward): Unit

    def actRoot(data: ModelData, modelRewards: ModelId => Distr): (ModelAction, ModelId) = {
        val modelsSorted = modelIds.map(modelId => {
                                        val reward = draw(modelRewards(modelId))
                                        (modelId, reward)
                                    }) 
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

    def actWithID(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
}


class EpsilonEnsembleThompsonSamplingLocal[ModelId, ModelData, ModelAction]
    (models: Map[ModelId, Model[ModelData, ModelAction]],
     newReward: (BetaDistribution[Reward], Reward) => BetaDistribution[Reward],
     evaluationFn: (ModelAction, ModelAction) => Reward,
     modelRewardsMap: MutableMap[ModelId, BetaDistribution[Reward]])
     extends EpsilonEnsembleThompsonSampling[ModelId, ModelData, ModelAction, BetaDistribution[Reward]](models)
     with EpsilonLearner[ModelId, ModelData, ModelAction, BetaDistribution[Reward]]{

    def getModelRewardsMap = modelRewardsMap
    val modelRewards = (modelId) => modelRewardsMap(modelId)

    def actWithID(data: ModelData): (ModelAction, ModelId) = actRoot(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelId, reward: Reward): Unit =  {modelRewardsMap(modelId).update(reward)}
    def learn(data: ModelData,
              correct: ModelAction,
              which: BetaDistribution[Reward] => Boolean = aggReward => true): Unit = learnRoot(modelIds, data, correct, which, evaluationFn, modelRewards)
}


object EpsilonEnsembleThompsonSamplingLocal {
    def apply[ModelId, ModelData, ModelAction](models: Map[ModelId, Model[ModelData, ModelAction]],
                newReward: (BetaDistribution[Reward], Reward) => BetaDistribution[Reward],
                evaluationFn: (ModelAction, ModelAction) => Reward,
                alpha: Double,
                beta: Double): EpsilonEnsembleThompsonSamplingLocal[ModelId, ModelData, ModelAction] = {
        val modelRewardsMap = MutableMap(models.keys.toList.map{key => (key,new BetaDistribution[Reward](alpha, beta))}:_*)
        new EpsilonEnsembleThompsonSamplingLocal[ModelId, ModelData, ModelAction](models,
            newReward,
            evaluationFn,
            modelRewardsMap)
    }
}