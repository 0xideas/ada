package ada.core.components.distributions

import ada._
import ada.core.interface.Exportable

sealed trait Distribution extends Exportable

trait SimpleDistribution extends Distribution{
    def draw: Double
    def update(reward: Reward): Unit
}

trait ContextualDistribution[Context] extends Distribution{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit
}
