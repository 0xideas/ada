package epsilon.core.components.distributions

import epsilon._
import epsilon.core.interface.Exportable

sealed trait Distribution extends Exportable

trait SimpleDistribution extends Distribution{
    def draw: Double
    def update(reward: Reward): Unit
}

trait ContextualDistribution[Context] extends Distribution{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit
}
