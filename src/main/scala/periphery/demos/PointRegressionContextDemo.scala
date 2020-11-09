package epsilon.demos

import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{ArrayBuffer => MutableList}

import epsilon.core.components.distributions.PointRegressionContext
import epsilon.core.ensembles._
import epsilon.core.models.DummyModel
import smile.data.DataFrame
import smile.data.formula._


object DemoPointRegressionContext{

    val model0 = new DummyModel(0.0)
    val model1 = new DummyModel(1.0)

    val r = scala.util.Random
    val initIndependent = DataFrame.of(Array.fill(50, 2){r.nextDouble})
    val initTarget =  DataFrame.of((Array.fill(25)(0.0) ++ Array.fill(25)(1.0)).map(Array(_)), "target")
    val initData = initIndependent.merge(initTarget)
    val regressionContext1 = new PointRegressionContext[Array[Double]]("target" ~, initData )
    val regressionContext2 = new PointRegressionContext[Array[Double]]("target" ~, initData )

    val ensemble = new EpsilonEnsembleGreedySoftmaxLocalWithContext[Int, Array[Double], Unit, Double, PointRegressionContext[Array[Double]]](
        Map(0 -> model0, 1 -> model1),
        MutableMap(0 -> regressionContext1, 1 -> regressionContext2),
        (context, aggregateReward) => aggregateReward.draw(context),
        0.2,
        (action1, action2) => math.exp(action1 - action2),
        (context,aggregateReward, reward) => {aggregateReward.update(context, reward); aggregateReward}
    )

    def run(): Unit = {
        println("started run")
        var i = 0
        while(i < 1000){
            ensemble.update(0, Array(1.0, 0.0, 1.0), 0.0)
            ensemble.update(0, Array(1.0, 1.0, 0.0), 1.0)

            ensemble.update(1, Array(1.0, 1.0, 0.0), 0.0)
            ensemble.update(1, Array(1.0, 0.0, 1.0), 1.0)
            i += 1
        }
        println("finished loop")

        val (action0, modelId0) = ensemble.actWithID(Array(1.0, 1.0, 0.0), ())
        val (action1, modelId1) = ensemble.actWithID(Array(1.0, 0.0, 1.0), ())
        println("single action")
        println(f"action0: $action0, modelId0: $modelId0")
        println(f"action1: $action1, modelId1: $modelId1")

        var actions0 = MutableList.empty[Double]
        var modelIds0 = MutableList.empty[Double]
        var actions1 = MutableList.empty[Double]
        var modelIds1 = MutableList.empty[Double]
        while( i < 2000){
            val (action0, modelId0) = ensemble.actWithID(Array(1.0, 1.0, 0.0), ())
            val (action1, modelId1) = ensemble.actWithID(Array(1.0, 0.0, 1.0), ())
            actions0 += action0
            modelIds0 += modelId0
            actions1 += action1
            modelIds1 += modelId1
            i += 1
        }
        println("1000 actions")
        println(f"action0: ${actions0.sum/1000}, modelId0: ${modelIds0.sum/1000}")
        println(f"action1: ${actions1.sum/1000}, modelId1: ${modelIds1.sum/1000}")


    }
}
