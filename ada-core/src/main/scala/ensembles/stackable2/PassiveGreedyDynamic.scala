package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta


class PassiveGreedyDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    evaluateFn: (ModelAction, ModelAction) => Reward)
    extends GreedyDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, 1.0)
    with PassiveEnsembleStackable[ModelID, ModelData, ModelAction, AggregateReward]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction) = {
            val updateFn = (modelIds: List[ModelID], context: ModelData, reward: Reward ) => this.update(modelIds, data, reward)
            _updateAllImplStackable(data, optimalAction, modelIds, models, modelKeys, modelRewards,  updateFn)
        } 
}



