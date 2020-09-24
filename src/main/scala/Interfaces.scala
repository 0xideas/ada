package epsilon

import scala.collection.mutable.{Map => MutableMap}
trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
}

abstract class EpsilonEnsembleInterface[ModelData, ModelAction, ModelId](epsilon: Double, models: Map[ModelId, Model[ModelData, ModelAction]]) {
    def act(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, correctAction: ModelAction): Reward
    def update(modelId: ModelId, reward: Reward): Unit
    def update(modelId: ModelId, action: ModelAction, correctAction: ModelAction): Unit

    def getModelId(model: Model[ModelData, ModelAction]): ModelId
    def getModel(modelId: ModelId): Model[ModelData, ModelAction]

}


