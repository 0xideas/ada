package ada.ensembles

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import breeze.stats.distributions.Beta
import io.circe.Decoder


class PassiveGreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: ModelID  => StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    evaluateFn: (ModelAction, ModelAction) => Reward)(implicit modelIdDecoder: Decoder[ModelID])
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, 0.0)
    with PassiveStackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward]{
        def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluateFn(action, optimalAction)
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
            _updateAllImplStackable1(data, optimalAction, modelIds, models, modelKeys, modelRewards)
        }
}





