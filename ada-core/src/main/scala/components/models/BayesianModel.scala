package components.models

import ada.core.components.distributions._
import ada.core.components.contextmodels._
import ada.core.interface._

abstract class BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta)
    with StackableModelPassiveBottom1[ModelID, Array[Double], Double, AggregateReward]{
    def predict(x: Array[Double]): Double
    def actWithID(data: Array[Double], selectedIds: List[ModelID]): (Double, List[ModelID]) = (predict(data), selectedIds)
    def update(modelIds: List[ModelID], data: Array[Double], reward: ada.Reward): Unit = update(data, reward)
    def update(modelIds: List[ModelID],reward: ada.Reward): Unit = ()

}


class BayesianMeanLinearRegressionModel[ModelID, AggregateReward](nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta) {
        def predict(x: Array[Double]): Double = predictProb(x).mean
}

class BayesianSampleLinearRegressionModel[ModelID, AggregateReward](nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta) {
        def predict(x: Array[Double]): Double = predictProb(x).sample
}