package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta


class PassiveGreedyDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModel2[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    evaluateFn: (ModelAction, ModelAction) => Reward)
    extends GreedyDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, 1.0)
    with PassiveEnsemble[ModelID, ModelData, ModelAction, AggregateReward]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
}




