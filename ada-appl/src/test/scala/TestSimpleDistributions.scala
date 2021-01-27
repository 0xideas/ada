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
import ada.core.components.distributions._
import _root_.breeze.stats.mode


class TestMeanDouble extends Properties("TestMeanDouble"){

    property("MeanDouble is within 1 +- 10e-5 of the mean") = Prop.forAll {(l: List[Double]) =>
        val meanDouble = new MeanDouble
        l.map(ll => meanDouble.update(new Reward(ll)))
        l.length == 0 || math.abs(1.0 - meanDouble.draw /(l.sum/l.length)) < 0.00001
    }
} 

class TestExpDouble extends Properties("TestExpDouble"){

    property("ExpDouble assignment works") = Prop.forAll {(v1: Double) =>
        val expDouble = new ExpDouble(v1)
        expDouble.draw== v1
    }

    property("ExpDouble value is last reward") = Prop.forAll {(v1: Double, vals: List[Double]) =>
        val expDouble = new ExpDouble(v1)
        vals.length == 0 || vals.map(v => {expDouble.update(new Reward(v)); expDouble.draw == v}).reduce(_&&_)
    }
}

class TestBetaDistribution extends Properties("TestBetaDistribution"){
    
    property("BetaDistribution with high alpha") = Prop.forAll{ (v1: Double, v2: Double) =>
        val v11 = math.abs(v1)
        val v22 = math.abs(v2)
        val distribution = new BetaDistribution(math.max(v11, v22)*2, math.min(v11, v22))
        (0 until 100).map(i => distribution.draw).sum > 50
    }

    property("BetaDistribution with high beta") = Prop.forAll{ (v1: Double, v2: Double) =>
        val vv1 = math.abs(v1)
        val vv2 = math.abs(v2)
        val distribution = new BetaDistribution(math.min(vv1, vv2)*2, math.max(vv1, vv2))
        (0 until 100).map(i => distribution.draw).sum < 50
    }

    property("BetaDistribution with low rewards") = Prop.forAllNoShrink{ (l: List[Double]) => 
        if(l.length > 10){
            val max_ = l.map(math.abs(_)).max 
            val ll = l.map(e => math.abs(e/max_))
            val distribution = new BetaDistribution(1, 1)
            ll.map(e => distribution.update(new Reward(math.pow(e, 2))))
            (0 until 100).map(i => distribution.draw).sum < 50
        }else{
            true
        }
    }
    
    property("BetaDistribution with high rewards") = Prop.forAllNoShrink{ (l: List[Double]) => 
        if(l.length > 10){
            val max_ = l.map(math.abs(_)).max 
            val ll = l.map(e => math.abs(e/max_))
            val distribution = new BetaDistribution(1, 1)
            ll.map(e => distribution.update(new Reward(1-math.pow(e, 2))))
            (0 until 100).map(i => distribution.draw).sum > 50
        }else{
            true
        }
    }
}
