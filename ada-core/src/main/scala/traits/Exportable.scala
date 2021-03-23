package ada.interface

import scala.collection.mutable.{Map => MutableMap}

import io.circe.Json
import ada.components.distributions.SimpleDistribution
import ada._



trait UpdateableContext[Context]{
    def update(context: Context, reward: ada.Reward): Unit
}

trait Updateable{
    def update(reward: ada.Reward): Unit
}
