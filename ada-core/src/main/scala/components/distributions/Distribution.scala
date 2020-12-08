package ada.core.components.distributions

import ada._
import ada.core.interface.{Model, Exportable}

sealed trait Distribution extends Exportable

trait SimpleDistribution 
    extends Distribution
    with Model[Unit, Double]{
    def draw: Double
    def update(reward: Reward): Unit

    def act(data: Unit = ()): Double = draw
}

trait ContextualDistribution[Context]
    extends Distribution
    with Model[Context, Double]{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit

    def act(data: Context): Double = draw(data)
}
