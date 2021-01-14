package components.models

import ada.core.components.distributions._
import ada.core.components.contextmodels._
import ada.core.interface._

abstract class BayesianLinearRegressionModelAbstract[ModelID](nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta)
    with StackableModel2[ModelID, Array[Double], Double]{
    def predict(x: Array[Double]): Double
    def actWithID(data: Array[Double], selectedIds: List[ModelID]): (Double, List[ModelID]) = (predict(data), selectedIds)
    def update(modelIds: List[ModelID], data: Array[Double], reward: ada.Reward): Unit = update(data, reward)
}


class BayesianMeanLinearRegressionModel[ModelID](nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionModelAbstract[ModelID](nfeatures, alpha, beta) {
        def predict(x: Array[Double]): Double = predictProb(x).mean
}

class BayesianSampleLinearRegressionModel[ModelID](nfeatures: Int, alpha: Double, beta: Double)
    extends BayesianLinearRegressionModelAbstract[ModelID](nfeatures, alpha, beta) {
        def predict(x: Array[Double]): Double = predictProb(x).sample
}