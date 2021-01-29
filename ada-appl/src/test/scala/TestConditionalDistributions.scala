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

trait hasConditionalDistribution{
    val rnd = scala.util.Random
    def conditionalDistributionGetter: ConditionalDistribution[Array[Double]]
}


trait hasTrainingDecreasesTest extends hasConditionalDistribution{
    def trainingDecreasesTest: Boolean = {
        val conditionalDistribution = conditionalDistributionGetter
        val context = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = context(0)*0.5 + context(1)*3 + rnd.nextGaussian()/2
        val predictionBefore = conditionalDistribution.draw(context)
        conditionalDistribution.update(context, new Reward(targetValue))
        val predictionAfter = conditionalDistribution.draw(context)
        math.abs(predictionAfter - targetValue) < math.abs(predictionBefore - targetValue)
    }
}

trait hasConvergenceTest extends hasConditionalDistribution{
    def convergenceTest: Boolean = {
        val conditionalDistribution = conditionalDistributionGetter
        val context = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = context(0) + context(1)
        (0 until 100000).map{j =>
            conditionalDistribution.update(context, new Reward(targetValue))
        }
        val prediction = conditionalDistribution.draw(context)
        math.abs(prediction-targetValue) < 0.3
    }
}

abstract class TestRegressionDistribution(name: String) extends Properties(name)
    with hasTrainingDecreasesTest
    with hasConvergenceTest{

    property(" training decreases absolute error") = Prop.forAllNoShrink {(i: Int) =>
        trainingDecreasesTest
    }

    property(" converges") = Prop.forAllNoShrink {(i: Int) =>
        convergenceTest
    } 
}

trait PointRegressionDistributionMount extends hasConditionalDistribution{
    val initIndependent = DataFrame.of(Array.fill(50, 1){rnd.nextDouble}, "1" )
    val initTarget =  DataFrame.of(Array.fill(50, 1){rnd.nextDouble}, "target")
    val initData = initIndependent.merge(initTarget)
    def conditionalDistributionGetter = new PointRegressionDistribution("target" ~, initData )

}

trait BayesianMeanRegressionDistributionMount{
    def conditionalDistributionGetter = new BayesianMeanRegressionDistribution(2)
}

class TestPointRegressionDistribution extends TestRegressionDistribution("TestPointRegressionDistribution")
                                    with PointRegressionDistributionMount

class TestBayesianMeanRegressionDistribution extends TestRegressionDistribution("TestBayesianMeanRegressionDistribution")
                                    with BayesianMeanRegressionDistributionMount


class TestBayesianSampleRegressionDistribution extends Properties("TestBayesianSampleRegressionDistribution"){
    val conditionalDistribution = new BayesianSampleRegressionDistribution(2)
    val conditionalDistributionComp = new BayesianMeanRegressionDistribution(2)
    val rnd = scala.util.Random

    property(" training updates identical to BayesianMeanRegressionDistribution") = Prop.forAllNoShrink {(i: Int) =>

        val context = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = context(0)*0.5 + context(1)*3 + rnd.nextGaussian()/2
        conditionalDistribution.update(context, new Reward(targetValue))
        conditionalDistributionComp.update(context, new Reward(targetValue))
        math.abs(conditionalDistributionComp.act(context)- (0 until 1000).map{j => conditionalDistribution.act(context)}.sum/1000) < 0.1

    }
}
