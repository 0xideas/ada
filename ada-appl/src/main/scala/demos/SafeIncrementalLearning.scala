package ada.demos


import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.PassiveGreedyEnsemble
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index
import ada.core.interface._

import plotting.Chart
import ada.generators.AutoregressionGenerator

import components.models.BayesianMeanLinearRegressionModel
import ada.core.components.contextmodels.BayesianSampleLinearRegression
import ada.core.components.distributions.BayesianSampleRegressionContext
import breeze.stats.distributions.Beta
import ada.core.components.distributions.MeanDouble

object SafeIncrementalLearning{
    val nIter= 100
    //initialise models
    val safeModelPath = "/home/leon/data/onnx/lr_autoregression5.onnx"
    val safeModel: StackableModelPassive[Int, Array[Array[Float]], Double, BayesianSampleRegressionContext] = new OnnxRegression[Int, Array[Array[Float]], Double, BayesianSampleRegressionContext](safeModelPath, "float_input")

    val models= 
        (0 until 4).map(i => new BayesianMeanLinearRegressionModel[Int, MeanDouble](5, 0.3, 1.0+i*3)).toList

    val rewards = (0 until 4).map(i => new MeanDouble(1.0))

    val ensemble = new PassiveGreedyEnsemble[Int, Array[Double], Double, MeanDouble](
                            models=(i: Int) => models(i),
                            modelKeys=() => (0 until 4).toList,
                            modelRewards=(0 until 4).map(i => (i, rewards(i))).toMap,
                            evaluateFn=(a: Double, b: Double) => math.pow(a-b, 2))
    
    val generator = new AutoregressionGenerator

    val values = ListBuffer((0 until 10).map(i => generator.next):_*)
    val errors:ListBuffer[ListBuffer[Double]] = ListBuffer.fill(7)(ListBuffer())
    val selectedModels = ListBuffer[Int]()

    def run(): Unit = {
        (0 until nIter-10).map{i =>
            val data = Array(values.takeRight(5):_*)
            val (action, modelIds) = ensemble.actWithID(data, List())
            val value = generator.next
            ensemble.updateAll(modelIds, data, value)
            println(f" ${action} | ${value}")
            errors(0).append(math.pow(0-value,2))
            errors(1).append(math.pow(values.takeRight(1)(0)-value,2))
            errors(2).append(math.pow(action-value,2))
            errors(3).append(math.pow(models(0).actWithID(data, List())._1-value,2))
            errors(4).append(math.pow(models(1).actWithID(data, List())._1-value,2))
            errors(5).append(math.pow(models(2).actWithID(data, List())._1-value,2))
            errors(6).append(math.pow(models(3).actWithID(data, List())._1-value,2))

            values.append(value)
            selectedModels.append(modelIds(0))
            ()
        }
        val chart = Chart(top=(1+values.max).toInt, bottom=(-1+values.min).toInt, left=0, right=nIter, width = 150, height = 40)
        chart.plotLine(values.toList, None, "+")
        println(chart.render())
        val selections = (0 until selectedModels.length).map{i =>
            f"${i+10}: ${selectedModels(i)}"
        }.mkString(" | ")
        println(selections)
        println(errors.map(err => err.toList.sum/err.length).mkString("\n"))
        println(ensemble.export)
    }
    


} 

