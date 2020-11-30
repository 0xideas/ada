package ada.core.models

import ada._

import io.circe.Json

import ada.core.interface.SimpleModel



class GenericStaticModel[ModelData, ModelAction](value: ModelAction)(implicit g: ModelAction => Json)
    extends SimpleModel[ModelData, ModelAction]{

    def act(data: ModelData): ModelAction = value

    def update(value: ModelAction): GenericStaticModel[ModelData, ModelAction] = 
        new GenericStaticModel[ModelData, ModelAction](value)

    override def toString: String = "$Model: " + value.toString() + "$"

    def update[ModelID](modelIds: List[ModelID], data: ModelData, reward: Reward): Unit = ()

    def export: Json = Json.fromFields(Map("value" -> g(value)))
} 


class StaticModel(value: Double) extends GenericStaticModel[Unit, Double](value)((d:Double) => Json.fromDouble(d).get)