package epsilon.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.learners._
import epsilon.core.components.distributions._



abstract class EpsilonEnsembleThompsonSamplingLocal
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     evaluationFn: (ModelAction, ModelAction) => Reward,
     modelRewards: MutableMap[ModelID, Distr])
     extends PassiveEnsemble[ModelID, ModelData, ModelAction, Distr]
     with EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr]
     with LocalEnsemble[ModelID, ModelData, ModelAction]
     with EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, Distr]{

    def actWithID(data: ModelData): (ModelAction, ModelID) =
        _actImpl(models, data, modelRewards)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = 
        evaluationFn(action, optimalAction)

    def updateAll(data: ModelData, correct: ModelAction): Unit = 
        _updateAllImpl(data, correct, models, modelRewards, this.update)

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

class EpsilonEnsembleThompsonSamplingLocalNoContext
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsembleThompsonSamplingLocal
     [ModelID, ModelData, ModelAction, Distr](models, evaluationFn, modelRewards)
     with EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, Distr]{
        def update(modelId: ModelID, reward: Reward): Unit = {modelRewards(modelId).update(reward)}
}


object EpsilonEnsembleThompsonSamplingLocalBeta {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double):
        EpsilonEnsembleThompsonSamplingLocalNoContext
        [ModelID, ModelData, ModelAction, BetaDistribution[Reward]] = {
        val modelRewardsMap = MutableMap(
            models.keys
                  .toList
                  .map{key => (key,new BetaDistribution[Reward](alpha, beta))}:_*
            )
        new EpsilonEnsembleThompsonSamplingLocalNoContext
            [ModelID, ModelData, ModelAction, BetaDistribution[Reward]](
            models, evaluationFn, modelRewardsMap)
    }
}


/*
abstract class EpsilonEnsembleThompsonSamplingLocalContextual[ModelID, ModelData, ModelAction, Context, Distr <: ContextualDistribution[Context, Reward]]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, Distr](models, evaluationFn, modelRewards){
        def update(modelId: ModelID, context: Context, reward: Reward): Unit =  {modelRewards(modelId).update(context, reward)}

}*/

/*
object ContextualThompsonSampling {
    def apply[ModelID, ModelData, ModelAction, Context]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward): 
         EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, RainierRegressionDistribution[Reward]] = {
        val modelRewardsMap = MutableMap(models.keys.toList.map{key => (key,new RainierRegressionDistribution[Reward])}:_*)


        new EpsilonEnsembleThompsonSamplingLocalContextual[ModelID, ModelData, ModelAction, Context, RainierRegressionDistribution[Reward]](models,
            evaluationFn,
            modelRewardsMap)
    }
}*/