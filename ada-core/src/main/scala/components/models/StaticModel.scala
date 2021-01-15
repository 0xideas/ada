package ada.core.models

import ada._

import io.circe.Json

import ada.core.interface._



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


class StaticModel[ModelID, ModelData, AggregateReward](value: Double) extends GenericStaticModel[ModelID, ModelData, Double, AggregateReward](value)((d:Double) => Json.fromDouble(d).get)