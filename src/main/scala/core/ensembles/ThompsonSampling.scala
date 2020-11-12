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
     val modelRewards: MutableMap[ModelID, Distr],     
     evaluationFn: (ModelAction, ModelAction) => Reward)
    extends EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, Distr](
        models,
        modelRewards,
        (distr: Distr) => distr.draw,
        0.0,
        evaluationFn,
        (distr: Distr, reward: Reward) => {distr.update(reward); distr}
    )


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
            models, modelRewardsMap, evaluationFn)
    }
}


class EpsilonEnsembleThompsonSamplingLocalWithContext
    [ModelID, Context, ModelData, ModelAction, ContextualDistr <: ContextualDistribution[Context]]
    (val models: Map[ModelID, Model[ModelData, ModelAction]],
     val modelRewards: MutableMap[ModelID, ContextualDistr],     
     evaluationFn: (ModelAction, ModelAction) => Reward)
    extends EpsilonEnsembleGreedySoftmaxLocalWithContext[ModelID, Context, ModelData, ModelAction, ContextualDistr](
        models,
        modelRewards,
        (context: Context, contextualDistr: ContextualDistr) => contextualDistr.draw(context),
        0.0,
        evaluationFn,
        (context: Context, contextualDistr: ContextualDistr,reward: Reward) => {contextualDistr.update(context, reward); contextualDistr}
    )
