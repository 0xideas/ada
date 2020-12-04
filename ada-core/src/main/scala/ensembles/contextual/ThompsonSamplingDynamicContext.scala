package ada.core.ensembles

import scala.collection.mutable.{Map => MutableMap}

import ada._
import ada.core.interface._
import ada.core.components.selectors._
import ada.core.components.distributions._
import breeze.stats.distributions.Beta

class ThompsonSamplingWithContext
    [ModelID, Context, ModelData, ModelAction, ContextualDistr <: ContextualDistribution[Context]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
     modelRewards: MutableMap[ModelID, ContextualDistr])
    extends GreedySoftmaxWithContext[ModelID, Context, ModelData, ModelAction, ContextualDistr](
        key => models(key),
        () => models.keys.toList,
        modelRewards,
        0.0
    )
