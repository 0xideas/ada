package epsilon.interfaces

import epsilon._

trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
}

abstract class EpsilonEnsembleInterface[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]], draw: AggregateReward => Double) extends Model[ModelData, ModelAction]{

    def getModel(id: ModelID): Model[ModelData, ModelAction]  = models(id)

    def act(data: ModelData): ModelAction = actWithID(data)._1
    def actWithID(data: ModelData): (ModelAction, ModelID)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelID, reward: Reward): Unit
    def update(modelId: ModelID, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, evaluate(action, optimalAction))

}

trait EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsembleInterface[ModelID, ModelData, ModelAction, AggregateReward]{
    def updateAll(data: ModelData,
            optimalAction: ModelAction): Unit

    def _updateAllImpl(modelIds: Iterable[ModelID],
                    data: ModelData,
                    optimalAction: ModelAction,
                    evaluationFn: (ModelAction, ModelAction) => Reward,
                    modelRewards: ModelID => AggregateReward): Unit = {
        modelIds.map(modelId => (modelId, modelRewards(modelId)))
                .map{    case(modelId, _) => (modelId, getModel(modelId)) }
                .map{    case(modelId, model) => {
                    val modelAction = model.act(data)
                    update(modelId, modelAction, optimalAction) 
                }}
    }
}

