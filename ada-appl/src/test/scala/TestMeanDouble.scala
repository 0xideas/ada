package ada

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import io.circe.Json

import ada.core.components.distributions.MeanDouble

import ada.core.models.{StaticModel, GenericStaticModel}
import ada.core.ensembles.GreedySoftmaxEnsemble
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.core.interface.{AdaEnsemble}
import ada.core.components.distributions.ExpDouble
import _root_.breeze.stats.mode


class TestMeanDouble extends Properties("TestMeanDouble"){

    property("MeanDouble is within 1 +- 10e-6 of the mean") = Prop.forAll {(l: List[Double]) =>
        val meanDouble = new MeanDouble
        l.map(ll => meanDouble.update(new Reward(ll)))
        l.length == 0 || math.abs(1.0 - meanDouble.draw /(l.sum/l.length)) < 0.000001
    }
} 