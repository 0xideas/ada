package epsilon

import scala.collection.mutable.{Map => MutableMap}
trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
}

abstract class EpsilonEnsembleInterface[ModelData, ModelAction](epsilon: Double, models: Iterable[Model[ModelData, ModelAction]]) {
    def update(model: Model[ModelData, ModelAction], reward: Reward): Unit
    def act(data: ModelData): ModelAction 
}


