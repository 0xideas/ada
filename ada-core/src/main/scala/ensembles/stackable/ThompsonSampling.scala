package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta


class ThompsonSamplingGeneric
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution]
    (models: ModelID  => StackableModel1[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
     modelRewards: Map[ModelID, Distr])
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, Distr](
        models,
        modelKeys,
        modelRewards,
        0.0
    )

class ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction]
    (models: Map[ModelID, StackableModel1[ModelID, ModelData, ModelAction]],
         alpha: Double,
         beta: Double)
    extends ThompsonSamplingGeneric[ModelID, ModelData, ModelAction, BetaDistribution](
        key => models(key),
        () => models.keys.toList,
        Map(models.keys.map(k => (k, new BetaDistribution(alpha, beta))).toSeq:_*))



