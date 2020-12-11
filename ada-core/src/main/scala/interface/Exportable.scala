package ada.core.interface

import scala.collection.mutable.{Map => MutableMap}

import io.circe.Json
import ada.core.components.distributions.SimpleDistribution
import ada._

trait Exportable{
    def export: Json
}

trait ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: Exportable]{
    def export(models: ModelID  => Model[ModelData, ModelAction],
               modelKeys: () => List[ModelID],
               modelRewards: Map[ModelID, AggregateReward]): Json = Json.fromFields(Map(
        "models" -> Json.fromFields(modelKeys().map{
            id => (id.toString(), models(id).export)
        }),
        "modelRewards" -> Json.fromFields(modelRewards.map{
            case(id, aggReward) => (id.toString(), aggReward.export)
        })
    ))
}
