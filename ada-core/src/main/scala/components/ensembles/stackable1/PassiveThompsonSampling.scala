package ada.ensembles

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import breeze.stats.distributions.Beta
import io.circe.Decoder


class PassiveThompsonSamplingEnsemble
    [ModelID, ModelData, ModelAction, Distr <: SimpleDistribution]
    (models: ModelID  => StackableModelPassive[ModelID, ModelData, ModelAction, Distr],
     modelKeys: () => List[ModelID],
     modelRewards: Map[ModelID, Distr],
    evaluateFn: (ModelAction, ModelAction) => Reward)(implicit modelIdDecoder: Decoder[ModelID])
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, Distr](
        models,
        modelKeys,
        modelRewards,
        0.0
    ) with PassiveStackableEnsemble1[ModelID, ModelData, ModelAction, Distr]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
            _updateAllImplStackable1(data, optimalAction, modelIds, models, modelKeys, modelRewards)
        }
}
