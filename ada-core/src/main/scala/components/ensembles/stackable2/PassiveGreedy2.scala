package ada.ensembles

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import breeze.stats.distributions.Beta


class PassiveGreedyEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ConditionalDistribution[ModelData]]
    (models: ModelID  => StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    evaluateFn: (ModelAction, ModelAction) => Reward)
    extends GreedyEnsemble2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, 1.0)
    with PassiveStackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction) = {
            _updateAllImplStackable2(data, optimalAction, modelIds, models, modelKeys, modelRewards)
        } 
}




