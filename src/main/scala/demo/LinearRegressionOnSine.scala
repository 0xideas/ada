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

    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = EpsilonEnsembleLearnerLocal[Double, Double, Int](0.0,
                                                               models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               (aggRew, rew) => rew,
                                                               evaluationFn)

    val generator = new SineGenerator(2.5, 24)

    
    def run(): Unit = {
        var next = 0.0
        var i = 0.0
        while(i < 1000){
            print("")
            next = generator.next
            val (action, selectedModel) = ensemble.act(1)
            ensemble.learn(1, next, aw => true)
            i = i + 1.0
            print(selectedModel.toString + " - ")
            //print((0.01*math.round(next*100)).toString + " - ")
            //print(models.map(model => (math.round(model.act(1)), math.round(evaluationFn(model.act(1), next)))).sortWith(_._1 < _._1).toString()  + " - ")
            //print(ensemble.models.values.toList.map(model => (math.round(model.act(1)), math.round(evaluationFn(model.act(1), next)))).sortWith(_._1 < _._1).toString()  + " - ")
            //print(ensemble.getModelRewardsMap.toList.sortBy(_._1))
            //print(ensemble.getModel(selectedModel).toString)
            //println()

            //map(x => math.round(x*100).toDouble/100))
            //println(ensemble.getModelRewardsMap)
            //print(next.toString + " - ")
        }
    }


}
