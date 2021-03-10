package ada.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._




class ThompsonSamplingEnsemble2
    [ModelID, ModelAction]
    (models: Map[ModelID, StackableModel[ModelID, Array[Double], ModelAction]],
     modelRewards: Map[ModelID, BayesianSampleRegressionDistribution])
    extends GreedyEnsemble2[ModelID, Array[Double], ModelAction, BayesianSampleRegressionDistribution](
        models,
        modelRewards,
        0.0
    )


object ThompsonSamplingEnsemble2{
    def apply[ModelID, ModelAction](
     models: Map[ModelID, StackableModel[ModelID, Array[Double], ModelAction]],
     modelRewards: Map[ModelID, BayesianSampleRegressionDistribution]) = {
         new ThompsonSamplingEnsemble2[ModelID, ModelAction](
             models,
             modelRewards)
    }
}