package ada.core.interface

import ada._

import io.circe.Json

trait Model[ModelData, ModelAction] extends Exportable{
    def report: String = this.toString
}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def update(data: ModelData, reward: Reward): Unit
    def act(data: ModelData): ModelAction
}

trait ContextualModel[Context, ModelData, ModelAction] 
    extends Model[ModelData, ModelAction]{
    def update(context: Context, data: ModelData, reward: Reward): Unit
    def act(context: Context, data: ModelData): ModelAction
}

trait StackableModel[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}

/*trait StackableModel2[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}
*/
trait StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateable]
    extends StackableModel[ModelID, ModelData, ModelAction]
    with PassiveEnsembleStackable1[ModelID, ModelData, ModelAction, AggregateReward]{
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit
}

/*
trait StackableModelPassive2[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[ModelData]]
    extends StackableModel[ModelID, ModelData, ModelAction]
    with PassiveEnsembleStackable2[ModelID, ModelData, ModelAction, AggregateReward]{
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit
}*/

trait StackableModelPassiveBottom[ModelID, ModelData, ModelAction]
    extends StackableModel[ModelID, ModelData, ModelAction]{
    def evaluate(action: ModelAction,optimalAction: ModelAction): ada.Reward
    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
        val (action, modelIds2) = actWithID(data, modelIds)
        update(modelIds, data, evaluate(action, optimalAction) )
    } 
}
/*
trait StackableModelPassiveBottom2[ModelID, ModelData, ModelAction, AggregateReward <: ExportUpdateableContext[ModelData]]
    extends StackableModelPassive2[ModelID, ModelData, ModelAction, AggregateReward]{
    def evaluate(action: ModelAction,optimalAction: ModelAction): ada.Reward
    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
        val (action, modelIds2) = actWithID(data, modelIds)
        update(modelIds, data, evaluate(action, optimalAction) )
    } 
}*/

