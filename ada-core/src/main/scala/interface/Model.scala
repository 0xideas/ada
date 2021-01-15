package ada.core.interface

import ada._

import io.circe.Json

trait Model[ModelData, ModelAction] extends Exportable{
    def report: String = this.toString
}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
}

trait ContextualModel[Context, ModelData, ModelAction] 
    extends Model[ModelData, ModelAction]{
    def act(context: Context, data: ModelData): ModelAction
}

trait StackableModel1[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}

trait StackableModel2[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}

trait StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModel1[ModelID, ModelData, ModelAction]
    with StackableModel2[ModelID, ModelData, ModelAction]
    with PassiveEnsembleStackable[ModelID, ModelData, ModelAction, AggregateReward]

trait StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]{
    def evaluate(action: ModelAction,optimalAction: ModelAction): ada.Reward = 0.0
    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
        val (action, modelIds2) = actWithID(data, modelIds)
        update(modelIds, data, evaluate(action, optimalAction) )
    } 
}