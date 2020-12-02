package ada.demos

import ada.core.models.SimpleLinearRegressionModel
import ada.core.ensembles.GreedySoftmaxLocal
import ada.generators.SineGenerator
import ada._
import ada.core.interface.ExpDouble

import scala.collection.mutable.{Map => MutableMap}

object DemoSine{
    val models = List(new SimpleLinearRegressionModel[Int](1, -3.0),
                    new SimpleLinearRegressionModel[Int](1, -2.0),
                    new SimpleLinearRegressionModel[Int](1, -1.0),
                    new SimpleLinearRegressionModel[Int](1, 0.0),
                    new SimpleLinearRegressionModel[Int](1, 1.0),
                    new SimpleLinearRegressionModel[Int](1, 2.0),
                    new SimpleLinearRegressionModel[Int](1, 3.0))




    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = GreedySoftmaxLocal[Int, Double, Double, ExpDouble](
                                                               models=models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               () => new ExpDouble(1.0),
                                                               0.2)

    val generator = new SineGenerator(2.8, 150)

    
    def run(): Unit = {
        var next = 0.0
        var i = 0.0
        while(i < 1000){
            print("")
            next = generator.next
            val (action, selectedModel) = ensemble.actWithID(-999, List())

            ensemble.update(selectedModel, action, next )
            //ensemble.learn(-999, next, aw => true)
            i = i + 1.0
            print(selectedModel.toString + "-")

        }
    }


}
