package epsilon.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.learners._
import epsilon.core.components.distributions._



class EpsilonEnsembleThompsonSamplingLocalNoContext
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, Distr]
     with PassiveEnsemble[ModelID, ModelData, ModelAction, Distr]
     with LocalEnsemble[ModelID, ModelData, ModelAction]
     with EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr]{

    def actWithID(data: ModelData): (ModelAction, ModelID) =
        _actImpl(models, data, modelRewards)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = 
        evaluationFn(action, optimalAction)

    def updateAll(data: ModelData, correct: ModelAction): Unit = 
        _updateAllImpl(data, correct, models, modelRewards, this.update)

    def update(modelId: ModelID, reward: Reward): Unit = {modelRewards(modelId).update(reward)}

    override def report: String = {
        val modelAttributes = (modelRewards.toList ++ 
            models.toList
                  .map{case(k,v) => (k, v.report)} )
                  .groupBy(_._1)
                  .map{case(k, v) => k -> v
                  .map(_._2)
                  .toSeq}
        modelAttributes.mkString("\n")
    }
}

object EpsilonEnsembleThompsonSamplingLocalNoContextBeta {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double):
        EpsilonEnsembleThompsonSamplingLocalNoContext
        [ModelID, ModelData, ModelAction, BetaDistribution] = {
        val modelRewardsMap = MutableMap(
            models.keys
                  .toList
                  .map{key => (key,new BetaDistribution(alpha, beta))}:_*
            )
        new EpsilonEnsembleThompsonSamplingLocalNoContext
            [ModelID, ModelData, ModelAction, BetaDistribution](
            models, evaluationFn, modelRewardsMap)
    }
}


