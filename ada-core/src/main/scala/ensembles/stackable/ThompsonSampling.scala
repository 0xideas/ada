package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta



class ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    alpha: Double,
    beta: Double,
    learningRate: Double = 1.0)
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, BetaDistribution](
        key => models(key),
        () => models.keys.toList,
        Map(models.keys.map(k => (k, new BetaDistribution(alpha, beta, learningRate))).toSeq:_*),
        0.0)



