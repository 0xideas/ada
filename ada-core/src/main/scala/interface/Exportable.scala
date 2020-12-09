package ada.core.interface

import scala.collection.mutable.{Map => MutableMap}

import io.circe.Json
import ada.core.components.distributions.SimpleDistribution
import ada._

trait Exportable{
    def export: Json
}
class ExpDouble(private var value: Double) extends SimpleDistribution {
    def export: Json = Json.fromDouble(value).get
    def draw: Double = value
    def update(reward: Reward): Unit = {value = reward; ()}
}
object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble(d) 
}

trait ExportableEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: Exportable]{
    def export(models: ModelID  => Model[ModelData, ModelAction],
               modelKeys: () => List[ModelID],
               modelRewards: MutableMap[ModelID, AggregateReward]): Json = Json.fromFields(Map(
        "models" -> Json.fromFields(modelKeys().map{
            id => (id.toString(), models(id).export)
        }),
        "modelRewards" -> Json.fromFields(modelRewards.map{
            case(id, aggReward) => (id.toString(), aggReward.export)
        })
    ))
}
