package epsilon.core.components.distributions

import epsilon._

sealed trait Distribution

trait SimpleDistribution extends Distribution{
    def draw: Double
    def update(reward: Reward): Unit
}

trait ContextualDistribution[Context] extends Distribution{
    def draw(context: Context): Double
    def update(context: Context, reward: Reward): Unit
}


