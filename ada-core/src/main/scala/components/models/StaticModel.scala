package ada.models

import ada._

import io.circe.Json

import ada.interface._



class GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward](var value: ModelAction)
    extends StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    with SimpleModel[ModelData, ModelAction]
    with InertModel[ModelID, ModelData, ModelAction]{

    def getValue: ModelAction = value
    def act(data: ModelData): LTree[ModelAction] = new LLeaf(value)

    def update(value: ModelAction): Unit = this.value = value

    override def toString: String = "$Model: " + value.toString() + "$"

    def actWithID(data: ModelData, selectedIds: LTree[ModelID]): (LTree[ModelAction], LTree[ModelID]) = (new LLeaf(value), selectedIds)
} 


class GenericStaticModelContext[ModelID, Context, ModelData, ModelAction, AggregateReward](value: ModelAction)
    extends GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward](value)
    with ContextualModel[ModelID, Context, ModelData, ModelAction]
    with InertModelContext[ModelID, Context, ModelData, ModelAction]{
        def actWithID(context: Context,data: ModelData, selectedIds: LTree[ModelID]): (LTree[ModelAction], LTree[ModelID]) = (new LLeaf(value), selectedIds)
}


class StaticModel[ModelID, ModelData, AggregateReward](value: Double) extends GenericStaticModel[ModelID, ModelData, Double, AggregateReward](value)

class StaticModelContext[ModelID, Context, ModelData, AggregateReward](value: Double) extends GenericStaticModelContext[ModelID, Context, ModelData, Double, AggregateReward](value)

class StaticModelString[ModelID, ModelData, AggregateReward](value: String) extends GenericStaticModel[ModelID, ModelData, String, AggregateReward](value)