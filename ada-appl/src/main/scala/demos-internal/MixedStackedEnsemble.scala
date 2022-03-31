package ada.demos

import ada.demos.utility.Utilities
import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.ensembles.{ThompsonSamplingEnsemble2, ThompsonSamplingEnsemble}
import ada.models._
import ada.components.distributions.{Distribution, BetaDistribution}
import ada.`package`.Reward
import ada.components.learners.BayesianSampleLinearRegression
import ada.components.distributions.BayesianSampleRegressionDistribution

object MixedStackedEnsemble{
    val nModels = 5
    val nFeatures = 3
    val ensembles = (0 until 3).map{i =>
        val models = (0 until nModels).map(x => new StaticModel[Int, Array[Double], BayesianSampleRegressionDistribution](x.toDouble)).toList
        val contexts = (0 until nModels).map(x => new BayesianSampleRegressionDistribution(nFeatures, 0.15, 1.0))
        ThompsonSamplingEnsemble2[Int,  Double](
            (0 until nModels).zip(models).toMap, Map((0 until nModels).zip(contexts):_*))
    }.toList ++ List(new StaticModel[Int, Array[Double], BayesianSampleRegressionDistribution](55.0))

    val ensemble = ThompsonSamplingEnsemble[Int, Array[Double], Double](
        (0 until ensembles.length).zip(ensembles).toMap,
         (0 until ensembles.length).map{i => (i, new BetaDistribution(1, 1, 1.0))}.toMap
    )
}