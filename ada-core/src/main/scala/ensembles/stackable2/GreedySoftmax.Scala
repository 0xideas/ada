package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}
import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._


class GreedySoftmaxDynamicEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ContextualDistribution[ModelData]]
    (models: ModelID  => StackableModel2[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    modelRewards: MutableMap[ModelID, AggregateReward],
    epsilon: Double)
    extends GreedyDynamicEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward](models, modelKeys, modelRewards, epsilon)
    with GreedySoftmax[ModelID, ModelData, ModelAction]
