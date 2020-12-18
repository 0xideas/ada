package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta
import ada.core.interface.PassiveEnsemble


class PassiveThompsonSamplingEnsemble
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
     modelRewards: Map[ModelID, Distr],
    evaluateFn: (ModelAction, ModelAction) => Reward)
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, Distr](
        models,
        modelKeys,
        modelRewards,
        0.0
    ) with PassiveEnsemble[ModelID, ModelData, ModelAction, Distr]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
}