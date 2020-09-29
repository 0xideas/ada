package epsilon

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.mutable.{Map => MutableMap}

import epsilon.models.{SimpleAutoRegressionModel, DummyModel}
import epsilon.ensembles.EpsilonEnsembleGreedySoftmaxLocal
import epsilon.generators.{AutoregressionGenerator, ConstantGenerator}
import epsilon.generators.TimeseriesGenerator
import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonLearner, Model}

class TestEpsilonEnsembleGreedySoftmax extends Properties("Test") {

    def isclose(n1: Double, n2: Double): Boolean = {
        math.abs(n1 - n2) <= (math.max(math.abs(n1), math.abs(n2)) * 0.1) + 0.05
    }

    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    
    def getConstObjects(const: Double, eta: Double): (TimeseriesGenerator, List[Model[Double, Double]], EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double]) = {
        val generator = new ConstantGenerator(const)

        val models = List(new DummyModel(const - 1.0),
                        new DummyModel(const),
                        new DummyModel(const - 1.0))

        val ensemble = EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double](eta,
                                                                models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                                (aggRew, rew) => rew,
                                                                evaluationFn,
                                                                aggRew => aggRew,
                                                                MutableMap(0 -> 1.0, 1 -> 1.0, 2 -> 3.0))
        return((generator, models, ensemble))
    }

    val constEta = for {
        const <- Gen.choose(1, 1000)
        etaSource <- Gen.choose(1, 1000)
    } yield((const.toDouble, math.abs(etaSource.toDouble/1500.0)))

    def report(eta: Double, rounds: List[(Double, Int)], tests:List[Boolean], ensemble: EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double]): Unit = {
        println("-----")
        println(ensemble.getModelRewardsMap)
        println(f"eta: $eta")
        println(rounds.map(_._2).groupBy(identity).mapValues(_.size).mapValues(1000*_/rounds.length).toMap)
        tests.map(println)
    }

    val nActions = 1000
    property("proportions of model selections correspond to eta value - initial reward") = forAll(constEta){
        tuple => {
            val (const, eta) = tuple

            require(eta >= 0)

            val (generator, models, ensemble) = getConstObjects(const, eta)
            
            val rounds = for {
                i <- (0 until nActions)
            } yield( ensemble.act(i))

            val test1 = isclose(rounds.count(_._2 == 2).toDouble, nActions*(1-eta))
            val test2 = isclose(rounds.count(t => t._2 == 0).toDouble/rounds.length, rounds.count(t => t._2 == 1).toDouble/rounds.length)

            if ((test1 && test2) == false) report(eta, rounds.toList, List(test1, test2), ensemble)

            test1 && test2
        }
    }

    property("proportions of model selections correspond to eta value - after learning") = forAll(constEta){
        tuple => {
            val (const, eta) = tuple
            require(eta >= 0)

            val (generator, models, ensemble) = getConstObjects(const, eta)

            ensemble.learn(0, const, any => true)

            val rounds = for {
                i <- (0 until nActions)
            } yield( ensemble.act(i))

            val test1 = isclose(rounds.count(_._2 == 1).toDouble, nActions*(1-eta))
            val test2 = isclose(rounds.count(t => t._2 == 0).toDouble/rounds.length, rounds.count(t => t._2 == 2).toDouble/rounds.length)

            if ((test1 && test2) == false) report(eta, rounds.toList, List(test1, test2), ensemble)

            test1 && test2
        }
    }

}