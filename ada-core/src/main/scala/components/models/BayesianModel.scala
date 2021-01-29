package ada.models

import ada.components.distributions._
import ada.components.learners._
import ada.interface._
import ada.`package`.Reward
trait BottomModel

abstract class BayesianLinearRegressionModelAbstract[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double, evaluationFn: (Double, Double)=>ada.Reward = (a, oA)=>new Reward(math.pow(a-oA, 2)))
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta)
    with StackableModelPassiveBottom[ModelID, Array[Double], Double, AggregateReward]{
    def predict(x: Array[Double]): Double
    def actWithID(data: Array[Double], selectedIds: List[ModelID]): (Double, List[ModelID]) = (predict(data), selectedIds)
    //def update(modelIds: List[ModelID], data: Array[Double], reward: ada.Reward): Unit = ()
    def update(modelIds: List[ModelID], data: Array[Double], optimalAction: Double): Unit = update(data, optimalAction)
    def update(modelIds: List[ModelID], data: Array[Double],  reward: ada.Reward): Unit = ()
    def evaluate(action: Double, optimalAction: Double): ada.Reward = evaluationFn(action, optimalAction)
}


class BayesianMeanRegressionModel[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double,  evaluationFn: (Double, Double)=>ada.Reward= (a, oA)=>new Reward(math.pow(a-oA, 2)))
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta, evaluationFn) {
        def predict(x: Array[Double]): Double = predictProb(x).mean
}

class BayesianSampleRegressionModel[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double,  evaluationFn: (Double, Double)=>ada.Reward= (a, oA)=>new Reward(math.pow(a-oA, 2)))
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta, evaluationFn) {
        def predict(x: Array[Double]): Double = predictProb(x).sample
}