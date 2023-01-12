package ada

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import io.circe.Json
import ada.components.distributions.MeanDouble
import breeze.linalg._

import ada.models.{StaticModel, GenericStaticModel}
import ada.ensembles.GreedySoftmaxEnsemble
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.interface.{AdaEnsemble}
import ada.components.distributions._
import _root_.breeze.stats.mode
import ada.interface._
import ada.models._
import ada.demos.utility.Utilities

trait hasModelGetter{
    val rnd = scala.util.Random
    def modelGetter: StackableModelPassiveBottom[Nothing, Array[Double], Double, Nothing]
}



trait modelTrainingDecreasesTest extends hasModelGetter{
    def trainingDecreasesTest: Boolean = {
        val model = modelGetter
        val data = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = Leaf(data(0)*0.5 + data(1)*3 + rnd.nextGaussian()/2)
        val predictionBefore = model.actWithID(data, Stub())._1
        model.update(data, targetValue)
        val predictionAfter = model.actWithID(data, Stub())._1
        math.abs(Utilities.extractSingleValue(predictionAfter) - Utilities.extractSingleValue(targetValue)) < math.abs(Utilities.extractSingleValue(predictionBefore) - Utilities.extractSingleValue(targetValue))
    }
}

trait modelConvergenceTest extends hasModelGetter{
    def convergenceTest: Boolean = {
        val model = modelGetter
        val data = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = Leaf(data(0) + data(1))
        (0 until 100000).map{j =>
            model.update(data, targetValue)
        }
        val prediction = model.actWithID(data, Stub())._1
        math.abs(Utilities.extractSingleValue(prediction)-Utilities.extractSingleValue(targetValue)) < 0.3
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
        math.abs(Utilities.extractSingleValue(modelComp.actWithID(data, Stub())._1) - (0 until 1000).map{j => Utilities.extractSingleValue(model.actWithID(data, Stub())._1)}.sum/1000) < 0.1

    }
}
