package ada.demos

import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{ListBuffer}

import ada.core.components.distributions.BayesianSampleRegressionContext
import ada.core.ensembles._
import ada.core.models.StaticModel


import plotting.Chart

object DemoBayesianRegressionContext{

    val model0 = new StaticModel(0.0)
    val model1 = new StaticModel(1.0)

    val regressionContext1 = new BayesianSampleRegressionContext(3, 0.3, 1.0 )
    val regressionContext2 = new BayesianSampleRegressionContext(3, 0.3, 1.0 )

    val ensemble = new ThompsonSamplingLocalWithContext[Int, Array[Double], Unit, Double,  BayesianSampleRegressionContext](
        Map(0 -> model0, 1 -> model1),
        MutableMap(0 -> regressionContext1, 1 -> regressionContext2),
        (action1, action2) => math.exp(action1 - action2)
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
        while(i < 2000){
            if(i % 10  == 0){
                var (share0, _, share1, _) = getAverages() 
                shares0 += share0
                shares1 += share1
            }
            if(i < 50){
                ()
            } else if(i < 250 || i > 750){
                ensemble.update(0, Array(1.0, 0.0, 1.0), 0.0)
                ensemble.update(0, Array(1.0, 1.0, 0.0), 3.0)
                ensemble.update(1, Array(1.0, 1.0, 0.0), 0.0)
                ensemble.update(1, Array(1.0, 0.0, 1.0), 3.0)
            } else {
                ensemble.update(1, Array(1.0, 0.0, 1.0), 0.0)
                ensemble.update(1, Array(1.0, 1.0, 0.0), 5.0)
                ensemble.update(0, Array(1.0, 1.0, 0.0), 0.0)
                ensemble.update(0, Array(1.0, 0.0, 1.0), 5.0)
            }
            i += 1
        }
        println("-----finished loop------")

        var (actions0, modelIds0, actions1, modelIds1) = getAverages()
        println("last values")
        println(f"action0: ${actions0}, modelId0: ${modelIds0}")
        println(f"action1: ${actions1}, modelId1: ${modelIds1}")


        println(Chart(1.1, -0.1, 0, 2000).plotLine(shares0.toList, Some("0"), "-").plotLine(shares1.toList, Some("1"), "+").render())


    }
}
