package ada.ensembles

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import breeze.stats.distributions.Beta

class ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelsMap: Map[ModelID, BetaDistribution],
    epsilon: Double = 0.0)
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, BetaDistribution](
        models,
        modelsMap,
        epsilon)

object ThompsonSamplingEnsemble{
    def apply[ModelID, ModelData, ModelAction] (
                models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                modelsMap: Map[ModelID, BetaDistribution],
                epsilon: Double = 0.0) = new ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction](
        models,
        modelsMap,
        epsilon)
}
