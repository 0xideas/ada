package ada.demos

import scala.collection.mutable.{ListBuffer}

import ada.core.models.{SimpleAutoRegressionModel, StaticModel}
import ada.core.ensembles.ThompsonSamplingLocalNoContextBeta
import ada.core.components.distributions.BetaDistribution
import ada.generators.AutoregressionGenerator
import ada._
import ada.core.components.distributions.{Distribution, BetaDistribution}

import plotting.Chart



object SparseThompsonSampling{
    //parameters for the demo
    val nIter = 1000 * 50
    val nFeatures = 5
    val nModels = 10
    val learningMultiplier = 5

    val conversionRate = Map(
        0 -> 0.013,
        1 -> 0.0173,
        2 -> 0.0174,
        3 -> 0.014,
        4 -> 0.0145,
        5 -> 0.011,
        6 -> 0.007,
        7 -> 0.0113,
        8 -> 0.0104,
        9 -> 0.0096
    )

    val rnd = scala.util.Random

    //initialisation of the ensemble
    val models = (0 until nModels).map(x => new StaticModel(x.toDouble))

    val ensemble = ThompsonSamplingLocalNoContextBeta[Int, Unit, Double](
                                                               (0 until nModels).zip(models).toMap,
                                                               (action1, action2) => math.pow(action1 - action2, 2),
                                                               100, 100)
    def getAverages(iter: Int = 100): List[Double] = {
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(()))).map(_._1)
        (0 until nModels).map{m => 
            selectedModels.toList.map(s => if(s == m) 1.0 else 0.0).sum / selectedModels.length
        }.toList
    }
    

    def report(shares: ListBuffer[ListBuffer[Double]]): Unit = {
        val characters = "abcdefghijklmnopqrst"

        val selections = getAverages(1000)

        selections.zipWithIndex.map{
            case(v, i) => println(f"action ${characters(i)}: $v")
        }

        var chart = Chart(1.1, -0.1, 0, nIter)
        (0 until nModels).map{i =>
            chart = chart.plotLine(shares(i).toList, Some(i.toString), characters(i%characters.length).toString())
        }
        println(chart.render())
    }

    
    def run(): ListBuffer[ListBuffer[Double]] = {
        print("-")
        var i = 0
        val selectedModels: ListBuffer[String] = ListBuffer.empty[String]
        val shares = ListBuffer.fill(nModels)(ListBuffer.empty[Double])

        while(i <  nIter){
            val (action, selectedModel) = ensemble.actWithID(())
            val reward = if(rnd.nextDouble() < conversionRate(selectedModel)) 1*learningMultiplier else -1*learningMultiplier
            ensemble.update(selectedModel, reward)
            if(i % scala.math.max(1, (nIter / 100).toInt) == 0){
                val averages = getAverages(100)
                shares.zipWithIndex.map{
                    case(s, i) => s += averages(i)
                }
            }
            i += 1

        }
        shares
    }

}
