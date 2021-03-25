package ada.ensembles

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import breeze.stats.distributions.Beta


class PassiveGreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: Map[ModelID, StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]],
    modelRewards: Map[ModelID, AggregateReward],
    evaluateFn: (LTree[ModelAction], LTree[ModelAction]) => Reward)
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards, 0.0)
    with PassiveStackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward]{
        def evaluate(action: LTree[ModelAction], optimalAction: LTree[ModelAction]): Reward = evaluateFn(action, optimalAction)
        def updateAll(modelIds: LTree[ModelID], data: ModelData, optimalAction: LTree[ModelAction]): Unit = {
            _updateAllImplStackable1(data, optimalAction, modelIds, models, modelRewards)
        }
}





