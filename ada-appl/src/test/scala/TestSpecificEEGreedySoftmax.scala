package ada

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import io.circe.Json


import ada.core.models.{StaticModel, GenericStaticModel}
import ada.core.ensembles.{GreedyEnsemble}
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.core.interface.{StackableEnsemble1}
import ada.core.components.distributions.ExpDouble
import _root_.breeze.stats.mode
import breeze.util.Index

trait TestEnsembleHelpers{
    def isclose(n1: Double, n2: Double): Boolean = {
        math.abs(n1 - n2) <= (math.max(math.abs(n1), math.abs(n2)) * 0.1) + 0.1
    }

    def makeGenerator[ModelId, ModelData, ModelAction](idGenerator: Gen[ModelId], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction],
                                                       makeEnsemble: (Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], List[Double], Double) => StackableEnsemble1[ModelId, ModelData, ModelAction, ExpDouble]) = {
        val generator = for{
            modelData <- dataGenerator
            const1 <- actionGenerator
            numIds <- Gen.chooseNum(6, 12)
            idsRaw <- Gen.listOfN(numIds, idGenerator)
            epsilonSource <- Gen.choose(10, 1000)
        } yield {
            val ids = idsRaw.toSet.toList
            println(ids.length)
            val epsilon = math.abs(epsilonSource.toDouble/1500.0)


            val modelMap = ids.map{id => (id, new GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble](const1)(x => Json.fromString(x.toString())))}.toMap

            val ensemble = makeEnsemble(modelMap, (0 until ids.length-1).map(i => 0.0).toList ++ List(1.0), epsilon)
            (epsilon, ids, ensemble, modelData)
        }
        generator
    }


    def report(epsilon: Double, rounds: List[(Any, Any)], tests: List[Boolean], rewardsMap: List[(Any, Any)]): Unit = {
        //println("-----")
        //println(rewardsMap.toMap)
        /*println(f"epsilon: $epsilon")
        println(rounds.map(_._2).groupBy(identity).mapValues(_.size).mapValues(1000*_/rounds.length).toMap)
        println(tests.mkString("-"))*/
    }

    def measureActionShares[ModelID, ModelData, ModelAction](data: ModelData, nActions: Int, ensemble: StackableEnsemble1[ModelID, ModelData, ModelAction, ExpDouble]) = {
        val rounds = for {
            i <- (0 until nActions)
        } yield{
            val (action, selectedIds) = ensemble.actWithID(data, List())
            (action, selectedIds(0))
        }
        rounds
    } 

    def testActionShares[ModelID, ModelData, ModelAction](rounds: IndexedSeq[(ModelAction, ModelID)], epsilon: Double, highRewardId: ModelID, otherIds: List[ModelID], ensemble: StackableEnsemble1[ModelID, ModelData, ModelAction, ExpDouble]) = {
        val highRewardIdAccordingToepsilon = isclose(rounds.count(_._2 == highRewardId).toDouble/rounds.length, rounds.length*(1-epsilon)/rounds.length)
        val otherIdsEquallyLikely = otherIds.map(id => isclose(rounds.count(t => t._2 == id).toDouble/rounds.length, rounds.count(t => t._2 == otherIds(0)).toDouble/rounds.length)).reduce(_ && _ )
        val result = highRewardIdAccordingToepsilon && otherIdsEquallyLikely

        if(result == false) report(epsilon, rounds.toList, List(highRewardIdAccordingToepsilon, otherIdsEquallyLikely), ensemble.modelRewards.toList)
        result
    }
}

trait hasMakeEnsembleFn{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double): StackableEnsemble1[ModelId,ModelData,ModelAction,ExpDouble]

}

abstract class TestBasis extends Properties("TestSpecificEEGreedySoftmax") 
    with TestEnsembleHelpers
    with hasMakeEnsembleFn {




    def testTypedEEGreedySoftmax[ModelID, ModelData, ModelAction](name: String, nActions: Int, idGenerator: Gen[ModelID], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction]) = {
        val generator = makeGenerator(idGenerator, dataGenerator, actionGenerator, makeEnsembleFn[ModelID, ModelData, ModelAction])

        property(name + " - proportions of model selections correspond to epsilon value - initial reward") = forAll(generator){
            tuple => {
                val (epsilon, ids, ensemble, modelData) = tuple

                val rounds = measureActionShares[ModelID, ModelData, ModelAction](modelData, nActions, ensemble)
                testActionShares[ModelID, ModelData, ModelAction](rounds, epsilon, ids.takeRight(1)(0), ids.take(ids.length-1), ensemble)
            }
        }
        
        property(name + " - proportions of model selections correspond to epsilon value - after learning") = forAll(generator){
            tuple => {
                val (epsilon, ids, ensemble, modelData) = tuple

                ensemble.update(List(ids.head), modelData, new Reward(1.0))
                ensemble.update(ids.takeRight(1), modelData, new Reward(0.0))

                val rounds = measureActionShares[ModelID, ModelData, ModelAction](modelData, nActions, ensemble)
                testActionShares[ModelID, ModelData, ModelAction](rounds, epsilon, ids.head, ids.tail, ensemble)
                
            }
        }
    }
    
    //these are the actual test executions

    testTypedEEGreedySoftmax("SDD", 1000, arbitrary[String], arbitrary[Double], arbitrary[Double])
    testTypedEEGreedySoftmax("SII", 1000, arbitrary[String], arbitrary[Int], arbitrary[Int])
    testTypedEEGreedySoftmax("IDIX", 1000, arbitrary[Int], arbitrary[Double], Gen.pick(5, (0 until 1000).toSet))

}

trait MakeEnsembleFnEpsilonGreedy{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double) = {
        new GreedyEnsemble[ModelId, ModelData, ModelAction, ExpDouble](
                                                                        (id) => modelMap(id),
                                                                        () => modelMap.keys.toList,
                                                                        modelMap.keys.zip(rewardValues).map{case(k, r) => (k, new ExpDouble(r))}.toMap,
                                                                        epsilon)
    }
}

class TestEpsilonGreedy extends TestBasis with MakeEnsembleFnEpsilonGreedy