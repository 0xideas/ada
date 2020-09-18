package epsilon


//reward must be positive
class EpsilonEnsemble(epsilon: Double,
                      models: Iterable[Model],
                      val update: (Model, Reward) => Unit,
                      modelRewards: Model => AggregateReward) extends EpsilonEnsembleInterface(epsilon, models) {
    
    def apply(epsilon: Double,
                models: Iterable[Model],
                update: (Model, Reward) => Unit,
                modelRewards: Model => AggregateReward): EpsilonEnsemble = 
        new EpsilonEnsemble(epsilon, models, update, modelRewards)

    private val rnd = new scala.util.Random

    def act(data: ModelData): ModelAction = {
        val modelsSorted = models.map(model => (model, modelRewards(model)))
                                 .toList
                                 .sortWith(_._2 > _._2)
        
        if(rnd.nextDouble() > epsilon) {println("exploiting..."); modelsSorted.head._1.act(data) }
        else explore(modelsSorted.tail, data)
    }

    def explore(modelsExplore: List[(Model, AggregateReward)], data: ModelData): ModelAction = {
        val totalReward: AggregateReward = modelsExplore.foldLeft(0.0)((agg, tup) => agg + tup._2)
        printEpsilon(totalReward.toString)
        print(modelsExplore.toString())
        val cumulativeProb: List[(Probability, Probability)] = modelsExplore.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        //Softmax
        val modelsCumulativeProb: List[(Model, (Probability, Probability))] = modelsExplore.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        val selectedModel: Model = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        printEpsilon("exploring... " + selectedModel.toString)
        selectedModel.act(data)
    }

}

class EpsilonEnsembleLocal(epsilon: Double,
                            models: Iterable[Model],
                            newReward: (AggregateReward, Reward) => AggregateReward) extends EpsilonEnsembleInterface(epsilon, models) {
    def apply(epsilon: Double,
              models: Iterable[Model],
              newReward: (AggregateReward, Reward) => AggregateReward): EpsilonEnsembleLocal = 
        new EpsilonEnsembleLocal(epsilon, models, newReward)
    
    val modelRewards: MutableMap[Model, AggregateReward] = MutableMap(models.toList.zip(List.fill(models.size)(1.0)):_*)

    val updateFunction: (Model, Reward) => Unit = (model, reward) => {modelRewards(model) = newReward(modelRewards(model), reward); ()}

    val epsilonEnsemble = new EpsilonEnsemble(epsilon, models, updateFunction, (model) => modelRewards(model))

    def act(data: ModelData): ModelAction = {println(modelRewards.toString); epsilonEnsemble.act(data)}

    def update(model: Model, reward: Reward): Unit = epsilonEnsemble.update(model, reward)
}

