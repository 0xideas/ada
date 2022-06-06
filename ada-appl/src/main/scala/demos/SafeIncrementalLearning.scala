package ada.demos

import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.ensembles.PassiveGreedyEnsemble
import ada.models._
import ada.components.distributions.{Distribution, BetaDistribution}
import ada.interface._
import ada.`package`.Reward
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import ada.demos.utility.Utilities

import plotting.Chart
import ada.generators.AutoregressionGenerator

import scala.collection.immutable.SortedMap

import ada.components.learners.BayesianSampleLinearRegression
import ada.components.distributions.BayesianSampleRegressionDistribution
import breeze.stats.distributions.Beta
import ada.components.distributions.MeanDouble
import com.lowagie.text.Utilities


object SafeIncrementalLearning{
    val nIter= 10000
    //initialise models
    //val safeModelPath = "/home/leon/data/onnx/lr_autoregression5.onnx"
    //val safeModel: StackableModelPassive[Int, Array[Array[Float]], Double, BayesianSampleRegressionDistribution] = new OnnxRegression[Int, Array[Array[Float]], Double, BayesianSampleRegressionDistribution](safeModelPath, "float_input", d => d)

    val staticModel = new StaticModel[Int, Array[Double], MeanDouble](0.0)
    val models= List(staticModel) ++
        (0 until 3).map(i => new BayesianMeanRegressionModel[Int, MeanDouble](5, 0.3, 1.0+(i*16))).toList

    val rewards = (0 until 4).map(i => new MeanDouble)

    implicit val evaluateFn = (a: Tree[Double], b: Tree[Double]) => {
        val step = 1.0-math.sqrt(math.abs(ada.demos.utility.Utilities.extractSingleValue(a)-ada.demos.utility.Utilities.extractSingleValue(b)))
        if( !(step.isNaN() || step.isInfinite()) && step > 0){new Reward(step)}
        else {/*println(f"a: ${a} b: ${b}");*/ new Reward(0.0)}
    }
    val ensemble = new PassiveGreedyEnsemble[Int, Array[Double], Double, MeanDouble](
                            (0 until 4).zip(models).toMap,
                            modelRewards_=(0 until 4).map(i => (i, rewards(i))).toMap,
                            evaluateFn=evaluateFn)

    

    //val getSetEnsemble = new ada.enhancements.GetSetEnsemble[Int, Array[Double], Double, MeanDouble, PassiveGreedyEnsemble[Int, Array[Double], Double, MeanDouble]]
    //import getSetEnsemble._


    val generator = new AutoregressionGenerator

    val values = ListBuffer((0 until 10).map(i => generator.next/5.0):_*)
    val errors:Map[String, ListBuffer[Double]] = SortedMap(List("predict 0.0", "predict last value", "ensemble prediction", "model 0", "model 1", "model 2", "model 3").map(i => (i, ListBuffer[Double]())):_*)
    val selectedModels = ListBuffer[Int]()

    val evaluateFn2 = (a1:Tree[Double], a2: Tree[Double]) => math.abs(ada.demos.utility.Utilities.extractSingleValue(a1) - ada.demos.utility.Utilities.extractSingleValue(a2))
    def run(): Unit = {
        (0 until nIter-10).map{i =>
            val data = Array(values.takeRight(5).toList:_*)
            val (action, modelIds) = ensemble.actWithID(data, Stub())
            val value = Leaf(generator.next/5.0)
            val rewardsNow = rewards.map(_.draw).map(_.toString).mkString(" - ")
            //println(rewardsNow)

            //println((0 until 4).map(j => models(j).actWithID(data, List())._1))
            if(i % 100 == 0) println(f"${i}: ${ada.demos.utility.Utilities.extractSingleValue(modelIds)}: ${action} - ${value}")
            ensemble.updateAll(modelIds, data, value)
            errors("predict 0.0").append(evaluateFn2(Leaf(0),value))
            errors("predict last value").append(evaluateFn2(Leaf(values.takeRight(1)(0)),value))
            errors("ensemble prediction").append(evaluateFn2(action,value))
            errors("model 0").append(evaluateFn2(models(0).actWithID(data, Stub())._1,value))
            errors("model 1").append(evaluateFn2(models(1).actWithID(data, Stub())._1,value))
            errors("model 2").append(evaluateFn2(models(2).actWithID(data, Stub())._1,value))
            errors("model 3").append(evaluateFn2(models(3).actWithID(data, Stub())._1,value))

            values.append(ada.demos.utility.Utilities.extractSingleValue(value))
            selectedModels.append( ada.demos.utility.Utilities.extractSingleValue(modelIds))
            ()
        }
        val valuesExt = values
        val chart = Chart(top=(valuesExt.max)*1.1, bottom=(+valuesExt.min)*1.1, left=0, right=nIter, width = 150, height = 40)
        chart.plotLine(valuesExt.toList, None, "+")
        println(chart.render())
        /*val selections = (0 until selectedModels.length).map{i =>
            f"${i+10}: ${selectedModels(i)}"
        }.mkString(" | ")
        println(selections)*/
        println(errors.map{case(label, errors) => f" $label: ${errors.toList.sum}"}.mkString("\n"))
    }
} 

