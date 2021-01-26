package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta



class ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    alpha: Double,
    beta: Double,
    learningRate: Double = 1.0,
    epsilon: Double = 0.0)
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, BetaDistribution](
        models,
        modelKeys,
        Map(modelKeys().map(k => (k, new BetaDistribution(alpha, beta, learningRate))).toSeq:_*),
        epsilon)

object ThompsonSamplingEnsemble{
    def apply[ModelID, ModelData, ModelAction] (
                models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                alpha: Double,
                beta: Double,
                learningRate: Double = 1.0) = new ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction](
        key => models(key),
        () => models.keys.toList,
        alpha,
        beta,
        learningRate,
        0.0)
}
