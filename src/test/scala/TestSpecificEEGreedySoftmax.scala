package epsilon

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.mutable.{Map => MutableMap}

import epsilon.models.{SimpleAutoRegressionModel, DummyModel, GenericDummyModel}
import epsilon.ensembles.EpsilonEnsembleGreedySoftmaxLocal
import epsilon.generators.{AutoregressionGenerator, ConstantGenerator}
import epsilon.generators.Generator
import epsilon.interfaces.{EpsilonEnsembleInterface, EpsilonLearner, Model}

class TestEpsilonEnsembleGreedySoftmax extends Properties("TestSpecificEEGreedySoftmax") {

    def isclose(n1: Double, n2: Double): Boolean = {
        math.abs(n1 - n2) <= (math.max(math.abs(n1), math.abs(n2)) * 0.1) + 0.05
    }

    //val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    def evaluationFn[ActionType](action: ActionType, correctAction: ActionType): Double =  if(action == correctAction) 1.0 else 0.1


    def getConstObjects(const: Double, eta: Double): (Generator[Double], List[Model[Double, Double]], EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double]) = {
        val generator = new ConstantGenerator(const)

        val models = List(new DummyModel(const - 1.0),
                        new DummyModel(const),
                        new DummyModel(const - 1.0))

        val ensemble = EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double](eta,
                                                                models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                                (aggRew, rew) => rew,
                                                                evaluationFn[Double],
                                                                aggRew => aggRew,
                                                                MutableMap(0 -> 1.0, 1 -> 1.0, 2 -> 3.0))
        return((generator, models, ensemble))
    }

    val constEta = for {
        const <- Gen.choose(1, 1000)
        etaSource <- Gen.choose(1, 1000)
    } yield((const.toDouble, math.abs(etaSource.toDouble/1500.0)))


    def makeGenerator[ModelId, ModelData, ModelAction](idGenerator: Gen[ModelId], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction]) = {
        val generator = for{
            modelData <- dataGenerator
            const1 <- actionGenerator
            const2 <- actionGenerator suchThat(_ != const1)
            id1 <- idGenerator
            id2 <- idGenerator suchThat(_ != id1)
            id3 <- idGenerator suchThat(id => id != id1 && id != id2)
            etaSource <- Gen.choose(1, 1000)
        } yield {
            val eta = etaSource.toDouble/1500.0

            val generator = new ConstantGenerator(const1)

            val models = List(new GenericDummyModel[ModelData, ModelAction](const1),
                          new GenericDummyModel[ModelData, ModelAction](const2),
                          new GenericDummyModel[ModelData, ModelAction](const2))

            val ensemble = EpsilonEnsembleGreedySoftmaxLocal[ModelId, ModelData, ModelAction, Double](eta,
                                                                    Map(id1 -> models(0), id2 -> models(1), id3 -> models(2)),
                                                                    (aggRew, rew) => rew,
                                                                    evaluationFn[ModelAction],
                                                                    aggRew => aggRew,
                                                                    MutableMap(id1 -> 1.0, id2 -> 1.0, id3 -> 3.0))
            
            (eta, (id1, id2, id3), (const1, const2), (generator, models, ensemble))
        }
        generator
    }

    private def report(eta: Double, rounds: List[(Any, Any)], tests:List[Boolean], rewardsMap: List[(Any, Any)]): Unit = {
        println("-----")
        println(rewardsMap.toMap)
        println(f"eta: $eta")
        println(rounds.map(_._2).groupBy(identity).mapValues(_.size).mapValues(1000*_/rounds.length).toMap)
        tests.map(println)
    }

    val generator =  makeGenerator(arbitrary[String], arbitrary[Double], arbitrary[Double])

    val nActions = 1000
    property("proportions of model selections correspond to eta value - initial reward") = forAll(generator){
        tuple => {
            val (eta, (id1, id2, id3), (const1, const2), (generator, models, ensemble)) = tuple
    
            val rounds = for {
                i <- (0 until nActions)
            } yield( ensemble.act(i))

            val test1 = isclose(rounds.count(_._2 == id3).toDouble, nActions*(1-eta))
            val test2 = isclose(rounds.count(t => t._2 == id1).toDouble/rounds.length, rounds.count(t => t._2 == id2).toDouble/rounds.length)
            val result = test1 && test2 

            if(result == false) report(eta, rounds.toList, List(test1, test2), ensemble.getModelRewardsMap.toList)
            result
        }
    }

    property("proportions of model selections correspond to eta value - after learning") = forAll(generator){
        tuple => {
            val (eta, (id1, id2, id3), (const1, const2), (generator, models, ensemble)) = tuple

            ensemble.learn(0, const1, any => true)

            val rounds = for {
                i <- (0 until nActions)
            } yield( ensemble.act(i))

            val test1 = isclose(rounds.count(_._2 == id1).toDouble, nActions*(1-eta))
            val test2 = isclose(rounds.count(t => t._2 == id2).toDouble/rounds.length, rounds.count(t => t._2 == id3).toDouble/rounds.length)
            val result = test1 && test2

            if (result == false) report(eta, rounds.toList, List(test1, test2), ensemble.getModelRewardsMap.toList)
            result
        }
    }

}