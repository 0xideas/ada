package ada.models

import ada._

import io.circe.Json

import ada.interface._



class GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward](private var value: ModelAction)(implicit g: ModelAction => Json)
    extends StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    with SimpleModel[ModelData, ModelAction]
    with InertModel[ModelID, ModelData, ModelAction]{

    def act(data: ModelData): ModelAction = value

    def update(value: ModelAction): Unit = this.value = value

    override def toString: String = "$Model: " + value.toString() + "$"

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = (value, selectedIds)

    def export: Json = Json.fromFields(Map("value" -> g(value)))
} 

class GenericStaticModelContext[ModelID, Context, ModelData, ModelAction, AggregateReward](private var value: ModelAction)(implicit g: ModelAction => Json)
    extends GenericStaticModel[ModelID, ModelData, ModelAction, AggregateReward](value)(g)
    with ContextualModel[ModelID, Context, ModelData, ModelAction]
    with InertModelContext[ModelID, Context, ModelData, ModelAction]{
        def actWithID(context: Context,data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = (value, selectedIds)
}


class StaticModel[ModelID, ModelData, AggregateReward](value: Double) extends GenericStaticModel[ModelID, ModelData, Double, AggregateReward](value)((d:Double) => Json.fromDouble(d).get)

class StaticModelContext[ModelID, Context, ModelData, AggregateReward](value: Double) extends GenericStaticModelContext[ModelID, Context, ModelData, Double, AggregateReward](value)((d:Double) => Json.fromDouble(d).get)

class StaticModelString[ModelID, ModelData, AggregateReward](value: String) extends GenericStaticModel[ModelID, ModelData, String, AggregateReward](value)((s:String) => Json.fromString(s))