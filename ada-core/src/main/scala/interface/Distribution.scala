package ada.components.distributions


import ada._
import ada.interface.{Model, ExportUpdateable, ExportUpdateableContext}

sealed trait Distribution

trait SimpleDistribution 
    extends Distribution
    with Model[Unit, Double]
    with ExportUpdateable{
    def draw: Double
    def update(reward: Reward): Unit

    def act(data: Unit = ()): Double = draw
}

trait ConditionalDistribution[Context]
    extends Distribution
    with Model[Context, Double]
    with ExportUpdateableContext[Context]{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit

    def act(data: Context): Double = draw(data)
}
