package ada

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import io.circe.Json


import ada.core.models.{StaticModel, GenericStaticModel}
import ada.core.ensembles.{GreedyEnsemble, GreedySoftmaxEnsemble}
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.core.interface.{StackableEnsemble1}
import ada.core.components.distributions.ExpDouble
import _root_.breeze.stats.mode
import breeze.util.Index

trait TestEnsembleHelpers{
    val rnd = scala.util.Random

    def isclose(n1: Double, n2: Double): Boolean = {
        //println(List(n1, n2).map(n => "f%.2f".format(n)).mkString(" "))
        math.abs(n1 - n2) <= 0.02  || 0.2 > 1.0 - math.min(n1, n2)/math.max(n1, n2)
    }

    def makeGenerator[ModelId, ModelData, ModelAction](idGenerator: Gen[ModelId], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction],
                                                       makeEnsemble: (Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], List[Double], Double) => StackableEnsemble1[ModelId, ModelData, ModelAction, ExpDouble]) = {
        val generator = for{
            modelData <- dataGenerator
            const1 <- actionGenerator
            numIds <- Gen.chooseNum(6, 8)
            idsRaw <- Gen.listOfN(numIds, idGenerator)
            rewardValues <- Gen.listOfN(numIds, arbitrary[Int])
            epsilonSource <- Gen.choose(100, 1000)
        } yield {
            val ids = idsRaw.toSet.toList
            val epsilon = math.abs(epsilonSource.toDouble/2000.0)
            val modelMap = ids.map{id => (id, new GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble](const1)(x => Json.fromString(x.toString())))}.toMap
            val rewardValues2 = rewardValues.map(r => math.abs(r.toDouble)).sorted
            val rewardValues3 = (rewardValues2.take(numIds - 2).map(r => r/5.0) ++ rewardValues2.drop(numIds-2).take(1).map(_/1.1) ++ rewardValues2.takeRight(1)).map(r => r/rewardValues2.max)
            //println(rewardValues3.map(n => "f%.2f".format(n)).mkString(" "))
            val ensemble = makeEnsemble(modelMap, rewardValues3 , epsilon)
            (epsilon, ids, ensemble, modelData)
        }
        generator
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
        val highRewardIdAccordingToepsilon = math.abs(rounds.count(_._2 == highRewardId).toDouble/rounds.length - (1-epsilon)) < 0.1
        val shares = otherIds.map(id => rounds.count(t => t._2 == id).toDouble/rounds.length)
        print(epsilon.toString + " ")
        println(shares ++ List(rounds.count(_._2 == highRewardId).toDouble/rounds.length))
        val comparisons = shares.flatMap(s => shares.map(t => 1.0 - math.min(s,t)/math.max(s,t)))
        val otherIdsEquallyLikely = comparisons.sum >= comparisons.length*0.1
        val result = highRewardIdAccordingToepsilon && otherIdsEquallyLikely
        //println((highRewardIdAccordingToepsilon, otherIdsEquallyLikely))
        (highRewardIdAccordingToepsilon, otherIdsEquallyLikely)
    }
}

trait hasMakeEnsembleFn{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double): StackableEnsemble1[ModelId,ModelData,ModelAction,ExpDouble]

}

abstract class TestBasis(label: String, equallyLikely: Boolean) extends Properties(label) 
    with TestEnsembleHelpers
    with hasMakeEnsembleFn {


    def testStackableEnsemble1[ModelID, ModelData, ModelAction](name: String, nActions: Int, idGenerator: Gen[ModelID], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction]) = {
        val generator = makeGenerator(idGenerator, dataGenerator, actionGenerator, makeEnsembleFn[ModelID, ModelData, ModelAction])

        property(name + " epsilon model selection") = forAll(generator){
            tuple => {
                val (epsilon, ids, ensemble, modelData) = tuple
                if(ids.length > 1){
                    val rounds = measureActionShares[ModelID, ModelData, ModelAction](modelData, nActions, ensemble)
                    val (accordingToEpsilon, otherIdsEquallyLikely) = testActionShares[ModelID, ModelData, ModelAction](rounds, epsilon, ids.takeRight(1)(0), ids.take(ids.length-1), ensemble)
                    accordingToEpsilon && (otherIdsEquallyLikely == equallyLikely)
                } else {
                    true
                }
            }
        }
        
        property(name + " epsilon model selection after training") = forAll(generator){
            tuple => {
                val (epsilon, ids, ensemble, modelData) = tuple

                if(ids.length > 1){
                    ids.map(id => ensemble.update(List(id), modelData, new Reward(0.0)))
                    ensemble.update(List(ids.head), modelData, new Reward(1.0))
                    val rounds = measureActionShares[ModelID, ModelData, ModelAction](modelData, nActions, ensemble)
                    val (accordingToEpsilon, otherIdsEquallyLikely) = testActionShares[ModelID, ModelData, ModelAction](rounds, epsilon, ids.head, ids.tail, ensemble)
                    accordingToEpsilon && (otherIdsEquallyLikely)

                } else {
                    true
                }
            }
        }
    }
    
    //these are the actual test executions

    testStackableEnsemble1("SDD", 10000, arbitrary[String], arbitrary[Double], arbitrary[Double])
    testStackableEnsemble1("SII", 10000, arbitrary[String], arbitrary[Int], arbitrary[Int])
    testStackableEnsemble1("IDIX", 10000, arbitrary[Int], arbitrary[Double], Gen.pick(5, (0 until 1000).toSet))

}

trait MakeEnsembleFnGreedy{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double) = {
        new GreedyEnsemble[ModelId, ModelData, ModelAction, ExpDouble](
                                                                        (id) => modelMap(id),
                                                                        () => modelMap.keys.toList,
                                                                        modelMap.keys.zip(rewardValues).map{case(k, r) => (k, new ExpDouble(r))}.toMap,
                                                                        epsilon)
    }
}

trait MakeEnsembleFnGreedySoftmax{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double) = {
        new GreedySoftmaxEnsemble[ModelId, ModelData, ModelAction, ExpDouble](
                                                                        (id) => modelMap(id),
                                                                        () => modelMap.keys.toList,
                                                                        modelMap.keys.zip(rewardValues).map{case(k, r) => (k, new ExpDouble(r))}.toMap,
                                                                        epsilon)
    }
}

class TestEpsilonGreedy extends TestBasis("GreedyEnsemble", true) with MakeEnsembleFnGreedy

class TestEpsilonGreedySoftmax extends TestBasis("GreedySoftmaxEnsemble", false) with MakeEnsembleFnGreedySoftmax