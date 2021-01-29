package ada

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import io.circe.Json
import smile.regression.{OnlineRegression, LinearModel}
import smile.regression.lm
import smile.data.formula.Formula
import smile.data.DataFrame
import ada.components.distributions.MeanDouble
import smile.data.formula._
import breeze.linalg._

import ada.models.{StaticModel, GenericStaticModel}
import ada.ensembles.GreedySoftmaxEnsemble
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.interface.{AdaEnsemble}
import ada.components.distributions._
import _root_.breeze.stats.mode
import ada.interface.StackableModelPassiveBottom
import ada.models._

trait hasModelGetter{
    val rnd = scala.util.Random
    def modelGetter: StackableModelPassiveBottom[Nothing, Array[Double], Double, Nothing]
}


trait modelTrainingDecreasesTest extends hasModelGetter{
    def trainingDecreasesTest: Boolean = {
        val model = modelGetter
        val data = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = data(0)*0.5 + data(1)*3 + rnd.nextGaussian()/2
        val predictionBefore = model.actWithID(data, List())._1
        model.update(data, targetValue)
        val predictionAfter = model.actWithID(data, List())._1
        math.abs(predictionAfter - targetValue) < math.abs(predictionBefore - targetValue)
    }
}

trait modelConvergenceTest extends hasModelGetter{
    def convergenceTest: Boolean = {
        val model = modelGetter
        val data = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = data(0) + data(1)
        (0 until 100000).map{j =>
            model.update(data, targetValue)
        }
        val prediction = model.actWithID(data, List())._1
        math.abs(prediction-targetValue) < 0.3
    }
}

abstract class TestRegressionModel(name: String) extends Properties(name)
    with modelTrainingDecreasesTest
    with modelConvergenceTest{

    property(" training decreases absolute error") = Prop.forAllNoShrink {(i: Int) =>
        trainingDecreasesTest
    }

    property(" converges") = Prop.forAllNoShrink {(i: Int) =>
        convergenceTest
    } 
}


trait BayesianMeanRegressionModelMount{
    def modelGetter = new BayesianMeanRegressionModel(2, 0.4, 1)
}


class TestBayesianMeanregressionModel extends TestRegressionModel("TestBayesianMeanregressionModel")
                                    with BayesianMeanRegressionModelMount


class TestBayesianSampleregressionModel extends Properties("TestBayesianSampleregressionModel"){
    val model = new BayesianSampleRegressionModel(2, 0.4, 1)
    val modelComp = new BayesianMeanRegressionModel(2, 0.4, 1)
    val rnd = scala.util.Random

    property(" training updates identical to BayesianMeanregressionModel") = Prop.forAllNoShrink {(i: Int) =>

        val data = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = data(0)*0.5 + data(1)*3 + rnd.nextGaussian()/2
        model.update(data, targetValue)
        modelComp.update(data, targetValue)
        math.abs(modelComp.actWithID(data, List())._1 - (0 until 1000).map{j => model.actWithID(data, List())._1}.sum/1000) < 0.1

    }
}
