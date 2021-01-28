package ada.demos

import ada.demos.utility.Utilities
import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.{ThompsonSamplingEnsemble2, ThompsonSamplingEnsemble}
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index
import ada.`package`.Reward
import ada.core.components.learners.BayesianSampleLinearRegression
import ada.core.components.distributions.BayesianSampleRegressionDistribution

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
        1.0, 1.0
    )
}