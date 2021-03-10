package ada.interface

import scala.collection.mutable.{Map => MutableMap}
import io.circe.Json

import ada._
import ada.interface._
import ada.components.distributions._
import ada.components.selectors.Selector


trait PassiveEnsemble[ModelData, ModelAction]{
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
}

trait PassiveSimpleEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]
    extends PassiveEnsemble[ModelData, ModelAction]{
    def _updateAllImplSimple(data: ModelData,
                       optimalAction: ModelAction,
                       models: Map[ModelID, SimpleModel[ModelData, ModelAction]],
                       modelRewards: Map[ModelID, AggregateReward]): Unit = {
        models.keys.toList.map{modelId => {
                val model = models(modelId)
                val modelAction = model.act(data)
                val reward = evaluate(modelAction, optimalAction)
                model.update(data, optimalAction)
                modelRewards(modelId).update(reward)
            }
        } 
    }

}

trait PassiveContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[Context]]
    extends PassiveEnsemble[ModelData, ModelAction]{
    
    def _updateAllImplContextual
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: Map[ModelID, ContextualModelPassive[ModelID, Context, ModelData, ModelAction, AggregateReward]],
                       modelRewards: Map[ModelID, AggregateReward],
                       context: Context): Unit = {
        models.keys.toList.map{modelId => {
                val model = models(modelId)
                val (modelAction, _) = model.actWithID(context, data, List())
                val reward = evaluate(modelAction, optimalAction)
                modelRewards(modelId).update(context, reward)
                model.updateAll(modelIds++List(modelId), context, data, optimalAction)

            }
        }
    }


}


trait PassiveStackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]
    extends PassiveEnsemble[ModelData, ModelAction]{

    def _updateAllImplStackable1
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: Map[ModelID, StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]],
                       modelRewards: Map[ModelID, AggregateReward]): Unit = {

        models.keys.toList.map{modelId => {
                val model = models(modelId)
                val (modelAction, _) = model.actWithID(data, List())
                val reward = evaluate(modelAction, optimalAction)
                modelRewards(modelId).update(reward)
                model.updateAll(modelIds++List(modelId), data, optimalAction)
            }
        } 
    }


}


trait PassiveStackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[ModelData]]
    extends PassiveEnsemble[ModelData, ModelAction]{

    def _updateAllImplStackable2
                       (data: ModelData,
                       optimalAction: ModelAction,
                       modelIds: List[ModelID],
                       models: Map[ModelID, StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]],
                       modelRewards: Map[ModelID, AggregateReward]): Unit = {

        models.keys.toList.map{modelId => {
                val model = models(modelId)
                val (modelAction, _) = model.actWithID(data, List())
                val reward = evaluate(modelAction, optimalAction)
                modelRewards(modelId).update(data, reward)
                model.updateAll(modelIds++List(modelId), data, optimalAction)
            }
        } 
    }


}