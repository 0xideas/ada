package ada.ensembles

import breeze.stats.mode

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import io.circe.Decoder


abstract class GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward],
    epsilon: Double)
    extends StackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with StackableActor1[ModelID, ModelData, ModelAction]{

    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): (Tree[ModelAction], Tree[ModelID]) = {
        _actImpl[AggregateReward](models, modelRewards, epsilon, data, selectedIds)
    }

}



class GreedyEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: SimpleDistribution]
    (models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward],
    protected[ada] var epsilon: Double)
    extends GreedyEnsembleAbstract[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards, epsilon)
    with GreedyRandom[ModelID, ModelData, ModelAction]

