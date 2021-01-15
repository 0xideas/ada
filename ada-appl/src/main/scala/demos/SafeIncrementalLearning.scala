package demos


import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.PassiveGreedyDynamicEnsemble
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index
import ada.core.interface._

import ada.generators.AutoregressionGenerator

import components.models.BayesianMeanLinearRegressionModel
import ada.core.components.contextmodels.BayesianSampleLinearRegression
import ada.core.components.distributions.BayesianSampleRegressionContext

object SafeIncrementalLearning{
    //initialise models
    val safeModel: StackableModelPassive[Int, Array[Double], Double, BayesianSampleRegressionContext] = new OnnxRegression[Int, Array[Double], Double, BayesianSampleRegressionContext](f"PATH.onnx", "input")

    val models: List[StackableModelPassive[Int, Array[Double], Double, BayesianSampleRegressionContext]] = 
        List(safeModel) ++ (0 until 3).map(i => new BayesianMeanLinearRegressionModel[Int, BayesianSampleRegressionContext](5, 1.0, 5.0+math.pow(i+1,2))).toList

    val rewards = (0 until 4).map(i => new BayesianSampleRegressionContext(5, 1, 5))

    val ensemble = new PassiveGreedyDynamicEnsemble[Int, Array[Double], Double, BayesianSampleRegressionContext](
                            models=(i: Int) => models(i),
                            modelKeys=() => (0 until 4).toList,
                            modelRewards=(0 until 4).map(i => (i, rewards(i))).toMap,
                            evaluateFn=(a: Double, b: Double) => math.pow(a-b, 2))
    
    val generator = new AutoregressionGenerator

    val values = ListBuffer((0 until 10).map(i => generator.next):_*)
    def run(): Unit = {
        (0 until 1000).map{i =>
            val data = Array(values.take(5):_*)
            val (action, modelIds) = ensemble.actWithID(data, List())
            val value = generator.next
            ensemble.updateAll(modelIds, data, value)
            ()
        }

    }

}

