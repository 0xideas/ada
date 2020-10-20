package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}


import epsilon.interfaces.{EpsilonEnsemblePassive, Model, LocalEnsemble}
import epsilon._
import epsilon.distributions.{Distribution, BetaDistribution}


trait EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr <: Distribution[Reward]]
    extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, Distr] {

    def draw: Distr => Double = (distr:Distr) => distr.draw

    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]], data: ModelData, modelRewards: ModelID => Distr): (ModelAction, ModelID) = {
        val modelsSorted = models.map{case(modelId,_) => {
                                        val reward = draw(modelRewards(modelId))
                                        (modelId, reward)
                                    }} 
                                    .toList
                                    .sortWith(_._2 > _._2)

        val selectedModelId = modelsSorted.head._1
        val selectedModel = models(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }     
}


class EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     updateAggregateRewardFn: (BetaDistribution[Reward], Reward) => BetaDistribution[Reward],
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, BetaDistribution[Reward]])
     extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, BetaDistribution[Reward]]
     with EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, BetaDistribution[Reward]]
     with LocalEnsemble[ModelID, ModelData, ModelAction,  BetaDistribution[Reward]]{

    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(models, data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelID, reward: Reward): Unit =  {modelRewards(modelId).update(reward)}
    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(data, correct, models, modelRewards)
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