package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.core.interface._
import ada.core.components.learners._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta


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

class ThompsonSamplingLocalNoContextBeta[ModelID, ModelData, ModelAction]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
         evaluationFn: (ModelAction, ModelAction) => Reward,
         alpha: Double,
         beta: Double)
    extends ThompsonSamplingLocalNoContext[ModelID, ModelData, ModelAction, BetaDistribution](
        models, MutableMap(models.keys.map(k => (k, new BetaDistribution(alpha, beta))).toSeq:_*), evaluationFn)

    /*
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
*/

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
