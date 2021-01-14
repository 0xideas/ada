package demos


import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.PassiveGreedyDynamicEnsemble
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index
import ada.core.interface._


import components.models.BayesianMeanLinearRegressionModel
import ada.core.components.contextmodels.BayesianSampleLinearRegression
import ada.core.components.distributions.BayesianSampleRegressionContext

object SafeIncrementalLearning{
    //initialise models
    val safeModel: StackableModel2[Int, Array[Double], Double] = new OnnxRegression[Int, Array[Double], Double](f"PATH.onnx", "input")

    val modelsL: List[StackableModel2[Int, Array[Double], Double]] = List(safeModel) ++ (0 until 3).map(i => new BayesianMeanLinearRegressionModel[Int](5, 1.0, 5.0+math.pow(i+1,2))).toList

    val rewards = (0 until 4).map(i => new BayesianSampleRegressionContext(5, 1, 5))

    val ensemble = new PassiveGreedyDynamicEnsemble[Int, Array[Double], Double, BayesianSampleRegressionContext](
                            models=(i: Int) => modelsL(i),
                            modelKeys=() => (0 until 4).toList,
                            modelRewards=(0 until 4).map(i => (i, rewards(i))).toMap,
                            evaluateFn=(a: Double, b: Double) => math.pow(a-b, 2))
    

    def run(): Unit = {

        ()

    }

}

