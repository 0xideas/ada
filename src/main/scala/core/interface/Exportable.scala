package epsilon.core.interface

import io.circe.Json

trait Exportable{
    def export: Json
}
class ExpDouble(val value: Double) extends Exportable {
    def export: Json = Json.fromDouble(value).get
}
object ExpDouble{
    implicit def expDouble: Double => ExpDouble = (d:Double) => new ExpDouble(d) 
}
