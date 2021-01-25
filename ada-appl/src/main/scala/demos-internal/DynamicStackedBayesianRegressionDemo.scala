package ada.demos

import ada.demos.utility.Utilities
import scala.collection.mutable.{ListBuffer}

import ada.core.components.distributions.BayesianSampleRegressionContext
import ada.core.ensembles._
import ada.core.models._
import ada.core.interface.StackableModel

import plotting.Chart
import ada.core.components.contextmodels.BayesianSampleLinearRegression

object StackedBayesianRegressionContextDemo{
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

    val ensembles = (0 until 3).map{i =>
        val models = (0 until nModels).map(x => new StaticModel[Int, Array[Double], BayesianSampleRegressionContext](x.toDouble)).toList
        val contexts = (0 until nModels).map(x => new BayesianSampleRegressionContext(nFeatures, 0.15, 1.0))

        
        new ThompsonSamplingDynamicLocal[Int, Array[Double], Double, BayesianSampleRegressionContext](
            (0 until nModels).zip(models).toMap, Map((0 until nModels).zip(contexts):_*))
    }

    val contexts = (0 until nModels).map(x => new BayesianSampleRegressionContext(nFeatures, 0.15, 1.0))
    val ensemble = new ThompsonSamplingDynamicLocal[Int, Array[Double], Double, BayesianSampleRegressionContext](
        (0 until nModels).zip(ensembles).toMap,
        Map((0 until nModels).zip(contexts):_*)
    )


    def run(): Unit = {

        val shares = Utilities.runStackable[Double, BayesianSampleRegressionContext](ensemble, highIndexMaps, nModels, nIter, nFeatures, 100, rnd, conversionRate)

        highIndexMaps.zipWithIndex.map{
            case(highIndexMap, f) => {
                val selections = Utilities.selectAndAverageDynamic[Double, BayesianSampleRegressionContext](ensemble, highIndexMap, nFeatures, nModels, rnd, 100)
                Utilities.report(highIndexMap, selections,  nModels, nIter, nFeatures, nGoodModels, shares(f))
            }
        }

    }
    val ensembles2 = (0 until 3).map{i =>
        val models = (0 until nModels).map(x => new StaticModelContext[Int, Array[Double], Unit, BayesianSampleRegressionContext](x.toDouble)).toList
        val contexts = (0 until nModels).map(x => new BayesianSampleRegressionContext(nFeatures, 0.15, 1.0))

        new ContextualThompsonSampling[Int, Unit, Double](
            (0 until nModels).zip(models).toMap, Map((0 until nModels).zip(contexts):_*))
    }

    val contexts2 = (0 until nModels).map(x => new BayesianSampleRegressionContext(nFeatures, 0.15, 1.0))
    val ensemble2 = new ContextualThompsonSampling[Int, Unit, Double](
        (0 until nModels).zip(ensembles2).toMap,
        Map((0 until nModels).zip(contexts2):_*)
    )

    def run2(): Unit = {

        val shares = Utilities.runContext[Double, BayesianSampleRegressionContext](ensemble2, highIndexMaps, nModels, nIter, nFeatures, 100, rnd, conversionRate)

        highIndexMaps.zipWithIndex.map{
            case(highIndexMap, f) => {
                val selections = Utilities.selectAndAverageContext[Double, BayesianSampleRegressionContext](ensemble2, nModels, highIndexMap, nFeatures,  rnd, 100)
                Utilities.report(highIndexMap, selections,  nModels, nIter, nFeatures, nGoodModels, shares(f))
            }
        }

    }
}
