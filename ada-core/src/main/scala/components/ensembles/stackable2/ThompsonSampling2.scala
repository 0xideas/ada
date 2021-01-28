package ada.core.ensembles

import breeze.stats.mode
import io.circe.Json

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._




class ThompsonSamplingEnsemble2
    [ModelID, ModelAction]
    (models: ModelID  => StackableModel[ModelID, Array[Double], ModelAction],
     modelKeys: () => List[ModelID],
     modelRewards: Map[ModelID, BayesianSampleRegressionDistribution])
    extends GreedyEnsemble2[ModelID, Array[Double], ModelAction, BayesianSampleRegressionDistribution](
        models,
        modelKeys,
        modelRewards,
        0.0
    )


object ThompsonSamplingEnsemble2{
    def apply[ModelID, ModelAction](
     models: Map[ModelID, StackableModel[ModelID, Array[Double], ModelAction]],
     modelRewards: Map[ModelID, BayesianSampleRegressionDistribution]) = {
         new ThompsonSamplingEnsemble2[ModelID, ModelAction](
             key => models(key),
             () => models.keys.toList,
             modelRewards)
    }
}