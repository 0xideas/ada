package ada.core.ensembles

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta
import ada.core.interface.PassiveEnsemble


class PassiveThompsonSamplingEnsemble
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution]
    (models: ModelID  => StackableModelPassive[ModelID, ModelData, ModelAction, Distr],
     modelKeys: () => List[ModelID],
     modelRewards: Map[ModelID, Distr],
    evaluateFn: (ModelAction, ModelAction) => Reward)
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, Distr](
        models,
        modelKeys,
        modelRewards,
        0.0
    ) with PassiveEnsembleStackable[ModelID, ModelData, ModelAction, Distr]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
            val updateFn = (modelIds: List[ModelID], context: ModelData, reward: Reward ) => this.update(modelIds, reward)
            _updateAllImplStackable(data, optimalAction, modelIds, models, modelKeys, modelRewards,  updateFn)
        }
}