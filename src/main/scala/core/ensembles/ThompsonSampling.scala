package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}


import epsilon.interfaces.{EpsilonEnsemblePassive, Model, LocalEnsemble}
import epsilon._
import epsilon.distributions.{Distribution, SimpleDistribution, BetaDistribution, ContextualDistribution}
import epsilon.interfaces._
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

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


abstract class EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     evaluationFn: (ModelAction, ModelAction) => Reward,
     modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, Distr]
     with EpsilonEnsembleThompsonSampling[ModelID, ModelData, ModelAction, Distr]
     with LocalEnsemble[ModelID, ModelData, ModelAction]
     with EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, Distr]{

    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(models, data, modelRewards)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)
    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(data, correct, models, modelRewards, this.update)

    override def report: String = {
        val modelAttributes = (modelRewards.toList ++ models.toList.map{case(k,v) => (k, v.report)} ).groupBy(_._1).map{case(k, v) => k -> v.map(_._2).toSeq}
        modelAttributes.mkString("\n")
    }
}

class EpsilonEnsembleThompsonSamplingLocalNoncontextual[ModelID, ModelData, ModelAction, Distr <: SimpleDistribution[Reward]]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, Distr](models, evaluationFn, modelRewards)
     with EpsilonEnsembleNoContext[ModelID, ModelData, ModelAction, Distr]{
        def update(modelId: ModelID, reward: Reward): Unit =  {modelRewards(modelId).update(reward)}

}

/*
abstract class EpsilonEnsembleThompsonSamplingLocalContextual[ModelID, ModelData, ModelAction, Context, Distr <: ContextualDistribution[Context, Reward]]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     val evaluationFn: (ModelAction, ModelAction) => Reward,
     val modelRewards: MutableMap[ModelID, Distr])
     extends EpsilonEnsembleThompsonSamplingLocal[ModelID, ModelData, ModelAction, Distr](models, evaluationFn, modelRewards){
        def update(modelId: ModelID, context: Context, reward: Reward): Unit =  {modelRewards(modelId).update(context, reward)}

}*/


object EpsilonEnsembleThompsonSamplingLocalBeta {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double): EpsilonEnsembleThompsonSamplingLocalNoncontextual[ModelID, ModelData, ModelAction, BetaDistribution[Reward]] = {
        val modelRewardsMap = MutableMap(models.keys.toList.map{key => (key,new BetaDistribution[Reward](alpha, beta))}:_*)
        new EpsilonEnsembleThompsonSamplingLocalNoncontextual[ModelID, ModelData, ModelAction, BetaDistribution[Reward]](models,
            evaluationFn,
            modelRewardsMap)
    }
}

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