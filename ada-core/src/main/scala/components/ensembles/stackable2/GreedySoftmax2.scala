package ada.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import io.circe.Decoder

class GreedySoftmaxEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ConditionalDistribution[ModelData]]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)(implicit modelIdDecoder: Decoder[ModelID])
    extends GreedyEnsembleAbstract2[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with GreedySoftmax[ModelID, ModelData, ModelAction]

