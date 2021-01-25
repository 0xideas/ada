package ada.demos

import ada.demos.utility.Utilities
import scala.collection.mutable.{ListBuffer}

import ada.core.components.distributions.BayesianSampleRegressionContext
import ada.core.ensembles._
import ada.core.interface.ContextualModel
import ada.core.models.{StaticModelContext}

import io.circe.Json

import plotting.Chart

object BayesianRegressionContextDemo{
    //parameters for the demo
    val nIter = 1000 * 10
    val nFeatures = 5
    val nModels = 3
    val nGoodModels = 2

    val conversionRate = Map((0 until nModels).map(k => (k, 0.1)):_*)
    /*val conversionRate = Map(
        0 -> 0.013,
        1 -> 0.017,
        2 -> 0.022
    )*/

    //highIndexMaps are the perturbations applied to the random context when taking a snapshot
    //this way the effect of new contexts on the ensemble can be studied

    //val highIndexMaps: List[Map[Int, Int]] = (0 until nFeatures).toList.map(v => Map(v -> 4))
    val highIndexMaps: List[Map[Int, Double]] = List(
        Map(0 -> 3.2),
        Map(1 -> 3.2),
        Map(2 -> 3.2),
        //Map(0 -> 3.2, 1 -> 3.2, 2 -> 3.2),
        Map()
    )

    val rnd = scala.util.Random

    //initialisation of the ensemble
    val models: IndexedSeq[ContextualModel[Int,Array[Double],Unit,Double]] = (0 until nModels).map(x => new StaticModelContext[Int, Array[Double], Unit, BayesianSampleRegressionContext](x.toDouble * 100000))
    
    val contexts = (0 until nModels).map(x => new BayesianSampleRegressionContext(nFeatures, 0.15, 1.0))
    val ensemble = new ContextualThompsonSampling[Int,  Unit, Double](
        (0 until nModels).zip(models).toMap,
        Map((0 until nModels).zip(contexts):_*)
    )


    def run(): Unit = {

        val shares = Utilities.runContext[Double, BayesianSampleRegressionContext](ensemble, highIndexMaps, nModels, nIter, nFeatures, 100, rnd, conversionRate)

        highIndexMaps.zipWithIndex.map{
            case(highIndexMap, f) => {
                val selections = Utilities.selectAndAverageContext[Double, BayesianSampleRegressionContext](ensemble, nModels, highIndexMap, nFeatures, rnd, 100)
                Utilities.report(highIndexMap, selections,  nModels, nIter, nFeatures, nGoodModels, shares(f))
            }
        }

    }
}
