package epsilon

import epsilon.SimpleLinearRegressionModel
import epsilon.EpsilonEnsembleLearnerLocal
import epsilon.SineGenerator

object Demo{
    val models = List(new SimpleLinearRegressionModel(-3.0),
                    new SimpleLinearRegressionModel(-2.0),
                    new SimpleLinearRegressionModel(-1.0),
                    new SimpleLinearRegressionModel(0.0),
                    new SimpleLinearRegressionModel(1.0),
                    new SimpleLinearRegressionModel(2.0),
                    new SimpleLinearRegressionModel(3.0))
    
    val ensemble = EpsilonEnsembleLearnerLocal[Double, Double, Int](0.2,
                                                               models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               (aggRew, rew)=>rew,
                                                               (action, correctAction) => math.max(0, 5-math.pow(action - correctAction ,2)))

    val generator = new SineGenerator(1.0, 1000)

    def run(): Unit = {
        var next = 0.0
        var i = 0
        while(i < 100){
            next = generator.next
            val (action, selectedModel) = ensemble.act(i)
            ensemble.update(selectedModel, action, next)
            i = i + 1
        }
    }


}
