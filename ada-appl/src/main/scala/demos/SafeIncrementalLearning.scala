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


import ada.core.components.contextmodels.BayesianSampleLinearRegression
import ada.core.components.distributions.BayesianSampleRegressionContext
import breeze.stats.distributions.Beta
import ada.core.components.distributions.MeanDouble

object SafeIncrementalLearning{
    val nIter= 1000000
    //initialise models
    val safeModelPath = "/home/leon/data/onnx/lr_autoregression5.onnx"
    val safeModel: StackableModelPassive[Int, Array[Array[Float]], Double, BayesianSampleRegressionContext] = new OnnxRegression[Int, Array[Array[Float]], Double, BayesianSampleRegressionContext](safeModelPath, "float_input")

    val staticModel = new StaticModel[Int, Array[Double], MeanDouble](0.0)
    val models= List(staticModel) ++
        (0 until 3).map(i => new BayesianMeanLinearRegressionModel[Int, MeanDouble](5, 0.3, 1.0+(i*16))).toList

    val rewards = (0 until 4).map(i => new MeanDouble(0.0))

    val evaluateFn = (a: Double, b: Double) => {
        val step = 1.0-math.sqrt(math.abs(a-b))
        if( !(step.isNaN() || step.isInfinite()) && step > 0){step}
        else {/*println(f"a: ${a} b: ${b}");*/ 0.0}
    }
    val ensemble = new PassiveGreedyEnsemble[Int, Array[Double], Double, MeanDouble](
                            models=(i: Int) => models(i),
                            modelKeys=() => (0 until 4).toList,
                            modelRewards=(0 until 4).map(i => (i, rewards(i))).toMap,
                            evaluateFn=evaluateFn)
    
    val generator = new AutoregressionGenerator

    val values = ListBuffer((0 until 10).map(i => generator.next/5.0):_*)
    val errors:ListBuffer[ListBuffer[Double]] = ListBuffer.fill(7)(ListBuffer())
    val selectedModels = ListBuffer[Int]()

    val evaluateFn2 = (a1:Double, a2: Double) => math.abs(a1 - a2)
    def run(): Unit = {
        (0 until nIter-10).map{i =>
            val data = Array(values.takeRight(5):_*)
            val (action, modelIds) = ensemble.actWithID(data, List())
            val value = generator.next/5.0
            val rewardsNow = rewards.map(_.draw).map(_.toString).mkString(" - ")
            //println(rewardsNow)

            //println((0 until 4).map(j => models(j).actWithID(data, List())._1))
            if(i % 100 == 0) println(f"${i}: ${modelIds(0)}: ${action} - ${value}")
            ensemble.updateAll(modelIds, data, value)
            errors(0).append(evaluateFn2(0,value))
            errors(1).append(evaluateFn2(values.takeRight(1)(0),value))
            errors(2).append(evaluateFn2(action,value))
            errors(3).append(evaluateFn2(models(0).actWithID(data, List())._1,value))
            errors(4).append(evaluateFn2(models(1).actWithID(data, List())._1,value))
            errors(5).append(evaluateFn2(models(2).actWithID(data, List())._1,value))
            errors(6).append(evaluateFn2(models(3).actWithID(data, List())._1,value))

            values.append(value)
            staticModel.update(value)
            selectedModels.append(modelIds(0))
            ()
        }
        val chart = Chart(top=(values.max)*1.1, bottom=(+values.min)*1.1, left=0, right=nIter, width = 150, height = 40)
        chart.plotLine(values.toList, None, "+")
        println(chart.render())
        /*val selections = (0 until selectedModels.length).map{i =>
            f"${i+10}: ${selectedModels(i)}"
        }.mkString(" | ")
        println(selections)*/
        println(errors.map(err => err.toList.sum).mkString("\n"))
        println(ensemble.export)
    }
    


} 

