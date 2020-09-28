package epsilon

trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
}

abstract class EpsilonEnsembleInterface[ModelId, ModelData, ModelAction, AggregateReward]
    (epsilon: Double, models: Map[ModelId, Model[ModelData, ModelAction]], draw: AggregateReward => Double) {
    def act(data: ModelData): (ModelAction, ModelId)
    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward
    def update(modelId: ModelId, reward: Reward): Unit

    def update(modelId: ModelId, action: ModelAction, optimalAction: ModelAction): Unit = update(modelId, evaluate(action, optimalAction))

    def getModelId(model: Model[ModelData, ModelAction]): ModelId
    def getModel(modelId: ModelId): Model[ModelData, ModelAction]

}


trait EpsilonLearner[ModelId, ModelData, ModelAction, AggregateReward]
    extends EpsilonEnsembleInterface[ModelId, ModelData, ModelAction, AggregateReward]{
    def learn(data: ModelData,
            correct: ModelAction,
            which: AggregateReward => Boolean ): Unit

    def learnRoot(modelIds: Iterable[ModelId],
                    data: ModelData,
                    optimalAction: ModelAction,
                    which: AggregateReward => Boolean,
                    evaluation: (ModelAction, ModelAction) => Reward,
                    modelRewards: ModelId => AggregateReward): Unit = {
        modelIds.map(modelId => (modelId, modelRewards(modelId)))
                .filter{ case(modelId, reward) => which(reward)}
                .map{    case(modelId, _) => (modelId, getModel(modelId)) }
                .map{    case(modelId, model) => {
                    val modelAction = model.act(data)
                    update(modelId, modelAction, optimalAction) 
                }}
    }
}

