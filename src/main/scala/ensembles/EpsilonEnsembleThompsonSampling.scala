package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}


import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonEnsemblePassive, Model}
import epsilon._
import epsilon.distributions.{Distribution, BetaDistribution}



abstract class EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr <: Distribution[Reward] ](
    models: Map[ModelID, Model[ModelData, ModelAction]], draw: Distr => Double = (distr:Distr) => distr.draw) 
    extends EpsilonEnsembleInterface(models, draw) {

    protected val modelIds: List[ModelID] = models.keys.toList

    def _actImpl(data: ModelData, modelRewards: ModelID => Distr): (ModelAction, ModelID) = {
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


class EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     updateAggregateRewardFn: (BetaDistribution[Reward], Reward) => BetaDistribution[Reward],
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, BetaDistribution[Reward]])
     extends EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, BetaDistribution[Reward]](models)
     with EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, BetaDistribution[Reward]]{


    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelID, reward: Reward): Unit =  {modelRewards(modelId).update(reward)}
    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(modelIds, data, correct, evaluationFn, modelRewards)
}


object EpsilonEnsembleThompsonSamplingLocal {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         updateAggregateRewardFn: (BetaDistribution[Reward], Reward) => BetaDistribution[Reward],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double): EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction] = {
        val modelRewardsMap = MutableMap(models.keys.toList.map{key => (key,new BetaDistribution[Reward](alpha, beta))}:_*)
        new EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction](models,
            updateAggregateRewardFn,
            evaluationFn,
            modelRewardsMap)
    }
}

class EpsilonEnsembleThompsonSamplingGeneral[ModelID, ModelData, ModelAction, Distr <: Distribution[Reward]](
                      models: Map[ModelID, Model[ModelData, ModelAction]],
                      modelRewards: ModelID => Distr,
                      evaluationFn: (ModelAction, ModelAction) => Reward ) extends EpsilonEnsembleThompsonSampling(models) {

    def update(modelId: ModelID, reward: Reward): Unit =  {modelRewards(modelId).update(reward)}

    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
}

