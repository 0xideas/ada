package ada.core.ensembles


import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta

class ContextualThompsonSampling
    [ModelID, ModelData, ModelAction]
    (models: Map[ModelID, ContextualModel[ModelID, Array[Double], ModelData, ModelAction]],
     modelRewards: Map[ModelID, BayesianSampleRegressionContext])
    extends ContextualGreedy[ModelID, Array[Double], ModelData, ModelAction, BayesianSampleRegressionContext](
        key => models(key),
        () => models.keys.toList,
        modelRewards,
        0.0
    )

    