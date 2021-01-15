package ada.core.models

import ada.core.components.distributions._
import ada.core.components.contextmodels._
import ada.core.interface._

trait BottomModel

abstract class BayesianLinearRegressionModelAbstract[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double, evaluationFn: (Double, Double)=>ada.Reward = (a, oA)=>math.pow(a-oA, 2))
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta)
    with StackableModelPassiveBottom[ModelID, Array[Double], Double, AggregateReward]
    with BottomModel{
    def predict(x: Array[Double]): Double
    def actWithID(data: Array[Double], selectedIds: List[ModelID]): (Double, List[ModelID]) = (predict(data), selectedIds)
    //def update(modelIds: List[ModelID], data: Array[Double], reward: ada.Reward): Unit = ()
    def update(modelIds: List[ModelID], data: Array[Double], optimalAction: Double): Unit = update(data, optimalAction)
    def update(modelIds: List[ModelID], reward: ada.Reward): Unit = ()
    def evaluate(action: Double, optimalAction: Double): ada.Reward = evaluationFn(action, optimalAction)
}


class BayesianMeanLinearRegressionModel[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double,  evaluationFn: (Double, Double)=>ada.Reward= (a, oA)=>math.pow(a-oA, 2))
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta, evaluationFn) {
        def predict(x: Array[Double]): Double = predictProb(x).mean
}

class BayesianSampleLinearRegressionModel[ModelID, ModelAction, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double,  evaluationFn: (Double, Double)=>ada.Reward= (a, oA)=>math.pow(a-oA, 2))
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta, evaluationFn) {
        def predict(x: Array[Double]): Double = predictProb(x).sample
}