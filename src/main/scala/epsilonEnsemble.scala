package epsilon

abstract class EpsilonEnsembleRoot[ModelData, ModelAction](epsilon: Double, models: Iterable[Model[ModelData, ModelAction]]) 
    extends EpsilonEnsembleInterface(epsilon, models) {

    def update(model: Model[ModelData, ModelAction], reward: Reward): Unit
    def act(data: ModelData): ModelAction 

    protected val rnd = new scala.util.Random
    def explore(modelsExplore: List[(Model[ModelData, ModelAction], AggregateReward)], data: ModelData): ModelAction = {
        val totalReward: AggregateReward = modelsExplore.foldLeft(0.0)((agg, tup) => agg + tup._2)
        printEpsilon(totalReward.toString)
        print(modelsExplore.toString())
        val cumulativeProb: List[(Probability, Probability)] = modelsExplore.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        //Softmax
        val modelsCumulativeProb: List[(Model[ModelData, ModelAction], (Probability, Probability))] = modelsExplore.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        //select by relative probability
        val selectedModel: Model[ModelData, ModelAction] = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        printEpsilon("exploring... " + selectedModel.toString)
        selectedModel.act(data)
    }

    def actRoot(data: ModelData, modelRewards: Model[ModelData, ModelAction] => AggregateReward): ModelAction = {
        val modelsSorted = models.map(model => (model, modelRewards(model)))
                                 .toList
                                 .sortWith(_._2 > _._2)
        if(rnd.nextDouble() > epsilon) {printEpsilon("exploiting..."); modelsSorted.head._1.act(data) }
        else explore(modelsSorted.tail, data)
    }
}


//reward must be positive
class EpsilonEnsemble[ModelData, ModelAction](epsilon: Double,
                      models: Iterable[Model[ModelData, ModelAction]],
                      updateFn: (Model[ModelData, ModelAction], Reward) => Unit,
                      modelRewards: Model[ModelData, ModelAction] => AggregateReward) extends EpsilonEnsembleRoot(epsilon, models) {

    def update(model: Model[ModelData, ModelAction], reward: Reward): Unit = updateFn(model, reward)
    def act(data: ModelData): ModelAction = actRoot(data, modelRewards)
}


class EpsilonEnsembleLocal[ModelData, ModelAction](epsilon: Double,
                            models: Iterable[Model[ModelData, ModelAction]],
                            newReward: (AggregateReward, Reward) => AggregateReward,
                            modelRewardsMap: MutableMap[Model[ModelData, ModelAction], AggregateReward]) extends EpsilonEnsembleRoot(epsilon, models) {

    def getModelRewardsMap = modelRewardsMap

    val modelRewards = (model) => modelRewardsMap(model)

    def update(model: Model[ModelData, ModelAction], reward: Reward): Unit = {
        modelRewardsMap(model) = newReward(modelRewards(model), reward)
    }

    def act(data: ModelData): ModelAction = actRoot(data, modelRewards)
}

object EpsilonEnsembleLocal {
    def apply[ModelData, ModelAction](epsilon: Double,
              models: Iterable[Model[ModelData, ModelAction]],
              newReward: (AggregateReward, Reward) => AggregateReward): EpsilonEnsembleLocal[ModelData, ModelAction] = {
        val modelRewardsMap = MutableMap(models.toList.zip(List.fill(models.size)(1.0)):_*)
        new EpsilonEnsembleLocal(epsilon, models, newReward, modelRewardsMap)
    }
    def apply[ModelData, ModelAction](epsilon: Double,
              models: Iterable[Model[ModelData, ModelAction]],
              newReward: (AggregateReward, Reward) => AggregateReward,
              modelRewardsMap: MutableMap[Model[ModelData, ModelAction], AggregateReward]): EpsilonEnsembleLocal[ModelData, ModelAction] = {
        new EpsilonEnsembleLocal(epsilon, models, newReward,  modelRewardsMap)
    }
}

