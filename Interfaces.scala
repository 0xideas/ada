package epsilon

trait Model {
    def act(data: ModelData): ModelAction
}

abstract class EpsilonEnsembleInterface (epsilon: Double,
                                            models: Iterable[Model],
                                            evaluate: ModelAction => Reward,
                                            rewardHistory: Model => Iterable[Reward],
                                            aggregateRewards: Iterable[Reward] => Reward) {
    def apply(epsilon: Double,
                models: Iterable[Model],
                evaluate: ModelAction => Reward,
                rewardHistory: Model => Iterable[Reward],
                aggregateRewards: Iterable[Reward] => Reward): EpsilonEnsembleInterface
    def act(data: ModelData): ModelAction
}

//reward must be positive
class EpsilonEnsemble(epsilon: Double,
                      models: Iterable[Model],
                      modelRewards: Model => Reward) extends EpsilonEnsembleInterface {
    
    private val rnd = new scala.util.Random

    def modelRewardsFromHistory(rewardHistory: Model => Iterable[Reward],
                                aggregateRewards: Iterable[Reward] => Reward): Model => Reward = {
        (aggregateRewards compose rewardHistory)
    }

    def apply(epsilon: Double,
                models: Iterable[Model],
                modelRewards: Model => Reward): EpsilonEnsemble = 
        new EpsilonEnsemble(epsilon, models, evaluate, rewardHistory, aggregateRewards)
    
    def act(data: ModelData): ModelAction = {
        val modelsSorted = models.map(model => (model, modelRewards(model)))
                                 .toList
                                 .sortWith(_._2 > _._2)
        
        if(rnd.nextDouble() > eps) modelsSorted.head._1.act(data)    
        else actSuboptimally(modelsSorted.tail)
    }

    def actSuboptimally(modelsSuboptimal: List[(Model, Reward)]): ModelAction = {
        val totalReward: Reward = modelsSuboptimal.foldLeft(0.0){case(model, reward) => _ + reward}
        val cumulativeProb: List[(Model, Reward)] = modelsSuboptimal.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward))
        val modelsCumulativeProb: List[(Model, Probability)] = modelsSuboptimal.map(_._1).zipWith(cumulativeProb)
        val selector = rnd.nextDouble()
        val selectedModel: Model = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        selectedModel.act(data)
    }
    
    
        
}