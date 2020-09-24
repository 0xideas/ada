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
    
    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 5-math.pow(action - correctAction ,2))
    val ensemble = EpsilonEnsembleLearnerLocal[Double, Double, Int](0.0,
                                                               models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               (aggRew, rew)=>rew,
                                                               evaluationFn)

    val generator = new SineGenerator(1.0, 8)

    def run(): Unit = {
        var next = 0.0
        var i = 0.0
        while(i < 25){
            next = generator.next
            val (action, selectedModel) = ensemble.act(0.3)
            ensemble.update(selectedModel, action, next)
            ensemble.learn(i, next, aw => true)
            i = i + 1.0
            println(selectedModel.toString + " - ")
            println(models.map(model => model.act(0.3)).map(action => evaluationFn(action, next)).map(x => math.round(x*100)))
            //print(next.toString + " - ")
        }
    }


}
