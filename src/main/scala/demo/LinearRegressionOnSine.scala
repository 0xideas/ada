package epsilon

import epsilon.models.SimpleAutoRegressionModel
import epsilon.ensembles.EpsilonEnsembleGreedySoftmaxLocal
import epsilon.generators.SineGenerator

object DemoSine{
    val models = List(new SimpleAutoRegressionModel(1, -3.0),
                    new SimpleAutoRegressionModel(1, -2.0),
                    new SimpleAutoRegressionModel(1, -1.0),
                    new SimpleAutoRegressionModel(1, 0.0),
                    new SimpleAutoRegressionModel(1, 1.0),
                    new SimpleAutoRegressionModel(1, 2.0),
                    new SimpleAutoRegressionModel(1, 3.0))


    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double](epsilon=0.2,
                                                               models=models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               newReward=(aggRew, rew) => rew,
                                                               evaluationFn=evaluationFn,
                                                               draw=(aggRew:Double) => aggRew,
                                                               initAggregateReward=0.0)

    val generator = new SineGenerator(2.8, 150)

    
    def run(): Unit = {
        var next = 0.0
        var i = 0.0
        while(i < 1000){
            print("")
            next = generator.next
            val (action, selectedModel) = ensemble.act(-999)
            ensemble.update(selectedModel, action, next )
            //ensemble.learn(-999, next, aw => true)
            i = i + 1.0
            print(selectedModel.toString + "-")

        }
    }


}
