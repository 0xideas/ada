package ada.components.distributions


import ada._
import ada.interface.{Model, Exportable, ExportUpdateable, ExportUpdateableContext, Settable}

sealed trait Distribution extends Exportable

trait SimpleDistribution 
    extends Distribution
    with Model[Unit, Double]
    with ExportUpdateable
    with Settable{
    def draw: Double
    def update(reward: Reward): Unit

    def act(data: Unit = ()): Double = draw
}

trait ConditionalDistribution[Context]
    extends Distribution
    with Model[Context, Double]
    with ExportUpdateableContext[Context]
    with Settable{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit

    def act(data: Context): Double = draw(data)
}
