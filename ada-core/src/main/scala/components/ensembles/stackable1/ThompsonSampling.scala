package ada.ensembles

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import breeze.stats.distributions.Beta
import io.circe.Decoder

class ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction]
    (models: ModelID  => StackableModel[ModelID, ModelData, ModelAction],
     modelKeys: () => List[ModelID],
    alpha: Double,
    beta: Double,
    learningRate: Double = 1.0,
    epsilon: Double = 0.0)(implicit modelIdDecoder: Decoder[ModelID])
    extends GreedyEnsemble[ModelID, ModelData, ModelAction, BetaDistribution](
        models,
        modelKeys,
        Map(modelKeys().map(k => (k, new BetaDistribution(alpha, beta, learningRate))).toSeq:_*),
        epsilon)

object ThompsonSamplingEnsemble{
    def apply[ModelID, ModelData, ModelAction] (
                models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
                alpha: Double,
                beta: Double,
                learningRate: Double = 1.0)(implicit modelIdDecoder: Decoder[ModelID]) = new ThompsonSamplingEnsemble[ModelID, ModelData, ModelAction](
        key => models(key),
        () => models.keys.toList,
        alpha,
        beta,
        learningRate,
        0.0)
}
