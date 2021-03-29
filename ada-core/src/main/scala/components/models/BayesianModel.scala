package ada.models

import ada.components.distributions._
import ada.components.learners._
import ada.interface._
import ada.`package`.Reward
trait BottomModel

abstract class BayesianLinearRegressionModelAbstract[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double, evaluationFn: (Tree[Double], Tree[Double]) => ada.`package`.Reward = BayesianRegressionEvaluationFn.evaluationFn)
    extends BayesianLinearRegressionAbstract(nfeatures, alpha, beta)
    with StackableModelPassiveBottom[ModelID, Array[Double], Double, AggregateReward]{
    def predict(x: Array[Double]): Double
    def actWithID(data: Array[Double], selectedIds: Tree[ModelID]): (Tree[Double], Tree[ModelID]) = (new Leaf(predict(data)), selectedIds)
    //def update(modelIds: Tree[ModelID], data: Array[Double], reward: ada.Reward): Unit = ()
    def update(modelIds: Tree[ModelID], data: Array[Double], optimalAction: Tree[Double]): Unit = update(data, optimalAction)
    def update(modelIds: Tree[ModelID], data: Array[Double],  reward: ada.Reward): Unit = ()
    def evaluate(action: Tree[Double], optimalAction: Tree[Double]): ada.Reward = evaluationFn(action, optimalAction)
}

object BayesianRegressionEvaluationFn{
    val evaluationFn = (action: Tree[Double], optimalAction: Tree[Double]) => {
        (action, optimalAction) match {
            case(Leaf(value1), Leaf(value2)) => new Reward(math.pow(value1-value2, 2))
            case(_, _) => throw new Exception("Could not compute reward for Bayesian regression from non Leaf Tree(s).")
        }

    }
}

class BayesianMeanRegressionModel[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double,  evaluationFn: (Tree[Double], Tree[Double]) => ada.`package`.Reward = BayesianRegressionEvaluationFn.evaluationFn)
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta, evaluationFn) {
        def predict(x: Array[Double]): Double = predictProb(x).mean
}

class BayesianSampleRegressionModel[ModelID, AggregateReward]
    (nfeatures: Int, alpha: Double, beta: Double, evaluationFn: (Tree[Double], Tree[Double]) => ada.`package`.Reward = BayesianRegressionEvaluationFn.evaluationFn)
    extends BayesianLinearRegressionModelAbstract[ModelID, AggregateReward](nfeatures, alpha, beta, evaluationFn) {
        def predict(x: Array[Double]): Double = predictProb(x).sample
}