package periphery.demos

import scala.collection.mutable.{Map => MutableMap}

import epsilon.core.components.distributions.PointRegressionContext
import epsilon.core.ensembles._
import epsilon.core.models.DummyModel
import smile.data.DataFrame
import smile.data.formula._


object DemoPointRegressionContext{

    val model0 = new DummyModel(0.0)
    val model1 = new DummyModel(1.0)

    val initIndependent = DataFrame.of(Array(Array.fill(2)(0.0), Array.fill(2)(1.0)))
    val initTarget = DataFrame.of(Array(Array(0.5), Array(0.5) ), "target")
    val initData = initIndependent.merge(initTarget)
    val regressionContext1 = new PointRegressionContext[Array[Double]]("class" ~, initData )
    val regressionContext2 = new PointRegressionContext[Array[Double]]("class" ~, initData )

    val ensemble = new EpsilonEnsembleGreedySoftmaxLocalWithContext[Int, Array[Double], Unit, Double, PointRegressionContext[Array[Double]]](
        Map(0 -> model0, 1 -> model1),
        MutableMap(0 -> regressionContext1, 1 -> regressionContext2),
        (context, aggregateReward) => aggregateReward.draw(context),
        0.2,
        (action1, action2) => math.exp(action1 - action2),
        (context,aggregateReward, reward) => {aggregateReward.update(context, reward); aggregateReward}
    )

    def run(): Unit = {
        var i = 0
        while(i < 1000){
            ensemble.update(0, Array(0.0, 1.0), 0.0)
            ensemble.update(0, Array(1.0, 1.0), 1.0)

            ensemble.update(1, Array(0.0, 1.0), 0.0)
            ensemble.update(1, Array(1.0, 1.0), 1.0)

            i += 1
        }

    }
}
