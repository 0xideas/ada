package ada.core.interface

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
