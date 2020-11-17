package ada.core.models
import io.circe.Json

import ada.core.interface.ModelNoContext



class GenericStaticModel[ModelData, ModelAction](value: ModelAction)(implicit g: ModelAction => Json)
    extends ModelNoContext[ModelData, ModelAction]{

    def act(data: ModelData): ModelAction = value

    def update(value: ModelAction): GenericStaticModel[ModelData, ModelAction] = 
        new GenericStaticModel[ModelData, ModelAction](value)

    override def toString: String = "$Model: " + value.toString() + "$"

    def export: Json = Json.fromFields(Map("value" -> g(value)))
} 


class StaticModel(value: Double) extends GenericStaticModel[Unit, Double](value)((d:Double) => Json.fromDouble(d).get)