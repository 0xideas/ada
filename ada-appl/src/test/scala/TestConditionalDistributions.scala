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
import ada.core.components.distributions.MeanDouble
import smile.data.formula._
import breeze.linalg._

import ada.core.models.{StaticModel, GenericStaticModel}
import ada.core.ensembles.GreedySoftmaxEnsemble
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.core.interface.{AdaEnsemble}
import ada.core.components.distributions._
import _root_.breeze.stats.mode

trait PointRegressionDistributionMount{

    val rnd = scala.util.Random
    val initIndependent = DataFrame.of(Array.fill(50, 1){rnd.nextDouble}, "1" )
    val initTarget =  DataFrame.of(Array.fill(50, 1){rnd.nextDouble}, "target")
    val initData = initIndependent.merge(initTarget)
}
class TestPointRegressionDistribution extends Properties("TestPointRegressionDistribution")
    with PointRegressionDistributionMount{

    property("PointRegressionDistribution training decreases absolute error") = Prop.forAll {(i: Int) =>
        val pointRegression = new PointRegressionDistribution("target" ~, initData )
        val context = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = context(0)*0.5 + context(1)*3 + rnd.nextGaussian()/2
        val predictionBefore = pointRegression.draw(context)
        pointRegression.update(context, new Reward(targetValue))
        val predictionAfter = pointRegression.draw(context)
        math.abs(predictionAfter - targetValue) < math.abs(predictionBefore - targetValue)
    }

    property("PointRegressionDistribution converges") = Prop.forAll {(i: Int) =>
        val pointRegression = new PointRegressionDistribution("target" ~, initData )
        val context = Array(rnd.nextDouble(), rnd.nextDouble())
        val targetValue = context(0) + context(1)
        (0 until 10000).map{j =>
            pointRegression.update(context, new Reward(targetValue))
        }
        val prediction = pointRegression.draw(context)
        math.abs(prediction-targetValue) < 0.3
    }
} 
