package epsilon.demos

import epsilon.models.SimpleAutoRegressionModel
import epsilon.ensembles.EpsilonEnsembleGreedySoftmaxLocal
import epsilon.generators.SineGenerator
import epsilon._

import scala.collection.mutable.{Map => MutableMap}

object DemoSine{
    val models = List(new SimpleAutoRegressionModel(1, -3.0),
                    new SimpleAutoRegressionModel(1, -2.0),
                    new SimpleAutoRegressionModel(1, -1.0),
                    new SimpleAutoRegressionModel(1, 0.0),
                    new SimpleAutoRegressionModel(1, 1.0),
                    new SimpleAutoRegressionModel(1, 2.0),
                    new SimpleAutoRegressionModel(1, 3.0))


    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = new EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double](epsilon=0.2,
                                                               models=models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               updateAggregateRewardFn=(aggRew:Double, rew:Reward) => rew,
                                                               evaluationFn=evaluationFn,
                                                               draw=(aggRew:Double) => aggRew,
                                                               modelRewards = MutableMap(models.zipWithIndex.toSeq.map{case(k,v) => (v, 1.0)}:_*),
)

    val generator = new SineGenerator(2.8, 150)

    
    def run(): Unit = {
        var next = 0.0
        var i = 0.0
        while(i < 1000){
            print("")
            next = generator.next
            val (action, selectedModel) = ensemble.actWithID(-999)
            ensemble.update(selectedModel, action, next )
            //ensemble.learn(-999, next, aw => true)
            i = i + 1.0
            print(selectedModel.toString + "-")

        }
    }


}
