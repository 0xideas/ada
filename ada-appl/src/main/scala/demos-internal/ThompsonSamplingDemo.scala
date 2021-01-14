package ada.demos

import scala.collection.mutable.{ListBuffer}

import ada.core.models.{StaticModel}
import ada.core.ensembles.{ThompsonSamplingEnsemble, ThompsonSamplingGeneric}
import ada.core.components.distributions.BetaDistribution
import ada._
import ada.core.components.distributions.{Distribution, BetaDistribution}

import plotting.Chart
import ada.core.interface.AdaEnsemble
import ada.core.components.distributions.BayesianSampleRegressionContext


object ThompsonSamplingDemo{
    def runSeveral(runs: Int = 25): Unit = {
        val results = (for{  _ <- 0 until runs} yield {
            val run = runOnce()
            run match {
                case(ensemble, nModels, nIter, shares) => {
                    val nGoodModels = nModels
                    val nFeatures = 1
                    val selections = Utilities.selectAndAverageStackable[Unit, Double, BetaDistribution](ensemble, (), nModels, 100)
                    Utilities.report(Map(), selections, nModels, nIter, nFeatures, nGoodModels, shares)
                } 
            }
            run
        }).toList

        type Share = ListBuffer[ListBuffer[Double]]
        val initB: Share = ListBuffer.fill(100)(ListBuffer.fill(100)(0.0))
        val reduced: Share = 
            results.map(_._4).foldLeft[Share](initB)( (agg: Share, shares: Share) => {
                agg.zipWithIndex.map{case (a,i) => (0 until a.length).map(j => a(j) += shares(i)(j)/runs )}; agg
            }) 
        
        //println("EXAMPLES")
        //results.take(5).map{case(ensemble, nModels, nIter, shares) => Demo.report(ensemble, nModels, nIter, shares)}
        println(f"EXPECTED VALUES OVER ${results.length} runs")

        Utilities.report(Map(), List(), results(0)._2, results(0)._3, 1, 1, reduced)
    }

    def runOnce(): (ThompsonSamplingEnsemble[Int, Unit, Double], Int, Int, ListBuffer[ListBuffer[Double]]) = {
        //parameters for the demo

        val nIter = 1000 * 100
        val nFeatures = 1
        val nModels = 5
        val nGoodModels = nModels
        val learningMultiplier = 15

        val conversionRate = Map(
            0 -> 0.013,
            1 -> 0.0143,
            2 -> 0.0198,
            3 -> 0.014,
            4 -> 0.0145,
            5 -> 0.011,
            6 -> 0.007,
            7 -> 0.0113,
            8 -> 0.0104,
            9 -> 0.0096
        ) //++ (10 until nModels).map(m => (m, 0.012)).toMap

        val rnd = scala.util.Random

        //initialisation of the ensemble
        val models = (0 until nModels).map(x => new StaticModel[Int, Unit](x.toDouble))


        val ensemble = new ThompsonSamplingEnsemble[Int, Unit, Double]((0 until nModels).zip(models).toMap, 100, 100)
        //stacked ensemble
        //val ensembles = (0 until 3).map{_ => new ThompsonSamplingLocalBeta[Int, Unit, Double]((0 until nModels).zip(models).toMap, 100, 100)}
        //val ensemble = new ThompsonSamplingLocalBeta[Int, Unit, Double]((0 until nModels).zip(models).toMap, 100, 100)

        

    
        print("-")
        var i = 0
        val selectedModels: ListBuffer[String] = ListBuffer.empty[String]
        val shares = ListBuffer.fill(nModels)(ListBuffer.empty[Double])

        while(i <  nIter){
            val (action, selectedModel) = ensemble.actWithID((), List())
            val reward = if(rnd.nextDouble() < conversionRate(selectedModel(0))) 1*learningMultiplier else -1*learningMultiplier
            ensemble.update(selectedModel, reward)
            if(i % scala.math.max(1, (nIter / 100).toInt) == 0){
                val averages = Utilities.selectAndAverageStackable[Unit, Double, BetaDistribution](ensemble, (), nModels, 1000)
                shares.zipWithIndex.map{
                    case(s, i) => s += averages(i)
                }
            }
            i += 1

        }
        val selections = Utilities.selectAndAverageStackable[Unit, Double, BetaDistribution](ensemble, (), nModels, 100)
        Utilities.report(Map(), selections, nModels, nIter, nFeatures, nGoodModels, shares)

        (ensemble, nModels, nIter, shares)
    }



    

}
