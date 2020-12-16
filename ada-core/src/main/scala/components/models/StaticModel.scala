package ada.core.models

import ada._

import io.circe.Json

import ada.core.interface._



class GenericStaticModel[ModelID, ModelData, ModelAction](value: ModelAction)(implicit g: ModelAction => Json)
    extends StackableModel[ModelID, ModelData, ModelAction]
    with StackableModel2[ModelID, ModelData, ModelAction]
    with SimpleModel[ModelData, ModelAction]{

    def act(data: ModelData): ModelAction = value

    def update(value: ModelAction): GenericStaticModel[ModelID, ModelData, ModelAction] = 
        new GenericStaticModel[ModelID, ModelData, ModelAction](value)

    override def toString: String = "$Model: " + value.toString() + "$"

    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = ()

    def update(modelIds: List[ModelID], reward: ada.Reward): Unit = ()

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = (value, selectedIds)

    def export: Json = Json.fromFields(Map("value" -> g(value)))
} 


class StaticModel[ModelID, ModelData](value: Double) extends GenericStaticModel[ModelID, ModelData, Double](value)((d:Double) => Json.fromDouble(d).get)