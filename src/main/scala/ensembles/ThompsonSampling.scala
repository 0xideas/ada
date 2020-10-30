package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}


import epsilon.interfaces.{EpsilonEnsemblePassive, Model, LocalEnsemble}
import epsilon._
import epsilon.distributions.{SimpleDistribution, BetaDistribution}
import distributions.RainierRegressionDistribution

trait EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
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


class EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     updateAggregateRewardFn: (Distr, Reward) => Distr,
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, Distr]
     with EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr]
     with LocalEnsemble[ModelID, ModelData, ModelAction, Distr]{

    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(models, data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def update(modelId: ModelID, reward: Reward): Unit =  {modelRewards(modelId).update(reward)}
    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(data, correct, models, modelRewards)

    override def report: String = {
        val modelAttributes = (modelRewards.toList ++ models.toList.map{case(k,v) => (k, v.report)} ).groupBy(_._1).map{case(k, v) => k -> v.map(_._2).toSeq}
        modelAttributes.mkString("\n")
    }
}



object EpsilonEnsembleThompsonSamplingLocalBeta {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         updateAggregateRewardFn: (BetaDistribution[Reward], Reward) => BetaDistribution[Reward],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double): EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, BetaDistribution[Reward]] = {
        val modelRewardsMap = MutableMap(models.keys.toList.map{key => (key,new BetaDistribution[Reward](alpha, beta))}:_*)
        new EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, BetaDistribution[Reward]](models,
            updateAggregateRewardFn,
            evaluationFn,
            modelRewardsMap)
    }
}
/*
object ContextualThompsonSampling {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward): 
         EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, RainierRegressionDistribution[Reward]] = {
        val modelRewardsMap = MutableMap(models.keys.toList.map{key => (key,new RainierRegressionDistribution[Reward])}:_*)

        val updateAggregateRewardFn = (model: RainierRegressionDistribution[Reward], reward: Reward) => {model.update(reward); model}

        new EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, RainierRegressionDistribution[Reward]](models,
            updateAggregateRewardFn,
            evaluationFn,
            modelRewardsMap)
    }
}*/