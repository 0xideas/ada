package epsilon.demos

import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{ListBuffer}

import epsilon.core.components.distributions.PointRegressionContext
import epsilon.core.ensembles._
import epsilon.core.models.StaticModel
import smile.data.DataFrame
import smile.data.formula._

import plotting.Chart

object DemoPointRegressionContext{

    val model0 = new StaticModel(0.0)
    val model1 = new StaticModel(1.0)

    val rnd = scala.util.Random
    val initIndependent = DataFrame.of(Array.fill(50, 2){rnd.nextDouble})
    val initTarget =  DataFrame.of((Array.fill(25)(0.0) ++ Array.fill(25)(1.0)).map(Array(_)), "target")
    val initData = initIndependent.merge(initTarget)
    val regressionContext1 = new PointRegressionContext("target" ~, initData )
    val regressionContext2 = new PointRegressionContext("target" ~, initData )

    val ensemble = new GreedySoftmaxLocalWithContext[Int, Array[Double], Unit, Double, PointRegressionContext](
        Map(0 -> model0, 1 -> model1),
        MutableMap(0 -> regressionContext1, 1 -> regressionContext2),
        (context, aggregateReward) => aggregateReward.draw(context),
        0.2,
        (action1, action2) => math.exp(action1 - action2),
        (context,aggregateReward, reward) => {aggregateReward.update(context, reward); aggregateReward}
    )

    def getAverages():(Double, Double, Double, Double) = {
        var (actions0, modelIds0, actions1, modelIds1)  = (ListBuffer.empty[Double], ListBuffer.empty[Double], ListBuffer.empty[Double], ListBuffer.empty[Double])
        var j = 0
        while( j < 1000) {
            val (action0, modelId0) = ensemble.actWithID(Array(1.0, 1.0, 0.0), ())
            val (action1, modelId1) = ensemble.actWithID(Array(1.0, 0.0, 1.0), ())
            actions0 += action0
            modelIds0 += modelId0
            actions1 += action1
            modelIds1 += modelId1
            j += 1
        }
        (actions0.sum/1000, modelIds0.sum/1000, actions1.sum/1000, modelIds1.sum/1000)
    }

    def run(): Unit = {
        println("started run")
        var i = 0
        var (shares0, shares1) =  (ListBuffer.empty[Double], ListBuffer.empty[Double])
        while(i < 100){
            if(i % 1  == 0){
                var (share0, _, share1, _) = getAverages() 
                shares0 += share0
                shares1 += share1
            }

            if(i > 25){
                ensemble.update(0, Array(1.0, 0.0, 1.0), 0.49)
                ensemble.update(0, Array(1.0, 1.0, 0.0), 0.51)
                ensemble.update(1, Array(1.0, 1.0, 0.0), 0.49)
                ensemble.update(1, Array(1.0, 0.0, 1.0), 0.51)
            } else {
                ensemble.update(1, Array(1.0, 0.0, 1.0), 0.49)
                ensemble.update(1, Array(1.0, 1.0, 0.0), 0.51)
                ensemble.update(0, Array(1.0, 1.0, 0.0), 0.49)
                ensemble.update(0, Array(1.0, 0.0, 1.0), 0.51)
            }
            i += 1
        }
        println("-----finished loop------")

        var (actions0, modelIds0, actions1, modelIds1) = getAverages()
        println("last values")
        println(f"action0: ${actions0}, modelId0: ${modelIds0}")
        println(f"action1: ${actions1}, modelId1: ${modelIds1}")

        println(Chart(1, 0, 0, 100).plotLine(shares0.toList, Some("0"), "-").plotLine(shares1.toList, Some("1"), "+").render())


    }
}
