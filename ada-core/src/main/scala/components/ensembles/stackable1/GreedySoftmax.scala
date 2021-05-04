package ada.ensembles

import breeze.stats.mode

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._


class GreedySoftmaxEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward],
    protected[ada] var epsilon: Double)
    extends GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards, epsilon)
    with GreedySoftmax[ModelID, ModelData, ModelAction]
    