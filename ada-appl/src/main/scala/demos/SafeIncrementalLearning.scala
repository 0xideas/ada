package ada.demos

import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.PassiveGreedyEnsemble
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index
import ada.core.interface._
import ada.`package`.Reward

import plotting.Chart
import ada.generators.AutoregressionGenerator

import scala.collection.immutable.SortedMap

import ada.core.components.learners.BayesianSampleLinearRegression
import ada.core.components.distributions.BayesianSampleRegressionDistribution
import breeze.stats.distributions.Beta
import ada.core.components.distributions.MeanDouble

object SafeIncrementalLearning{
    val nIter= 10000
    //initialise models
    val safeModelPath = "/home/leon/data/onnx/lr_autoregression5.onnx"
    val safeModel: StackableModelPassive[Int, Array[Array[Float]], Double, BayesianSampleRegressionDistribution] = new OnnxRegression[Int, Array[Array[Float]], Double, BayesianSampleRegressionDistribution](safeModelPath, "float_input")

    val staticModel = new StaticModel[Int, Array[Double], MeanDouble](0.0)
    val models= List(staticModel) ++
        (0 until 3).map(i => new BayesianMeanRegressionModel[Int, MeanDouble](5, 0.3, 1.0+(i*16))).toList

    val rewards = (0 until 4).map(i => new MeanDouble)

    val evaluateFn = (a: Double, b: Double) => {
        val step = 1.0-math.sqrt(math.abs(a-b))
        if( !(step.isNaN() || step.isInfinite()) && step > 0){new Reward(step)}
        else {/*println(f"a: ${a} b: ${b}");*/ new Reward(0.0)}
    }
    val ensemble = new PassiveGreedyEnsemble[Int, Array[Double], Double, MeanDouble](
                            models=(i: Int) => models(i),
                            modelKeys=() => (0 until 4).toList,
                            modelRewards=(0 until 4).map(i => (i, rewards(i))).toMap,
                            evaluateFn=evaluateFn)
    
    val generator = new AutoregressionGenerator

    val values = ListBuffer((0 until 10).map(i => generator.next/5.0):_*)
    val errors:Map[String, ListBuffer[Double]] = SortedMap(List("predict 0.0", "predict last value", "ensemble prediction", "model 0", "model 1", "model 2", "model 3").map(i => (i, ListBuffer[Double]())):_*)
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
            errors("predict 0.0").append(evaluateFn2(0,value))
            errors("predict last value").append(evaluateFn2(values.takeRight(1)(0),value))
            errors("ensemble prediction").append(evaluateFn2(action,value))
            errors("model 0").append(evaluateFn2(models(0).actWithID(data, List())._1,value))
            errors("model 1").append(evaluateFn2(models(1).actWithID(data, List())._1,value))
            errors("model 2").append(evaluateFn2(models(2).actWithID(data, List())._1,value))
            errors("model 3").append(evaluateFn2(models(3).actWithID(data, List())._1,value))

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
        println(errors.map{case(label, errors) => f" $label: ${errors.toList.sum}"}.mkString("\n"))
    }
} 

