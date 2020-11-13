package epsilon.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.learners._
import epsilon.core.components.distributions._


class ThompsonSamplingLocalNoContext
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     modelRewards: MutableMap[ModelID, Distr],     
     evaluationFn: (ModelAction, ModelAction) => Reward)
    extends GreedySoftmaxLocal[ModelID, ModelData, ModelAction, Distr](
        models,
        modelRewards,
        (distr: Distr) => distr.draw,
        0.0,
        evaluationFn,
        (distr: Distr, reward: Reward) => {distr.update(reward); distr}
    )


object ThompsonSamplingLocalNoContextBeta {
    def apply[ModelID, ModelData, ModelAction]
        (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double):
        ThompsonSamplingLocalNoContext
        [ModelID, ModelData, ModelAction, BetaDistribution] = {
        val modelRewardsMap = MutableMap(
            models.keys
                  .toList
                  .map{key => (key,new BetaDistribution(alpha, beta))}:_*
            )
        new ThompsonSamplingLocalNoContext
            [ModelID, ModelData, ModelAction, BetaDistribution](
            models, modelRewardsMap, evaluationFn)
    }
}


class ThompsonSamplingLocalWithContext
    [ModelID, Context, ModelData, ModelAction, ContextualDistr <: ContextualDistribution[Context]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     modelRewards: MutableMap[ModelID, ContextualDistr],     
     evaluationFn: (ModelAction, ModelAction) => Reward)
    extends GreedySoftmaxLocalWithContext[ModelID, Context, ModelData, ModelAction, ContextualDistr](
        models,
        modelRewards,
        (context: Context, contextualDistr: ContextualDistr) => contextualDistr.draw(context),
        0.0,
        evaluationFn,
        (context: Context, contextualDistr: ContextualDistr,reward: Reward) => {contextualDistr.update(context, reward); contextualDistr}
    )
