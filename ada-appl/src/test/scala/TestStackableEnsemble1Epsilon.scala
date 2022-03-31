package ada

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import io.circe.Json


import ada.models.{StaticModel, GenericStaticModel}
import ada.ensembles.{GreedyEnsemble, GreedySoftmaxEnsemble}
import ada.generators.{ConstantGenerator}
import ada.generators.Generator
import ada.interface.{StackableEnsemble1}
import ada.components.distributions.ExpDouble
import _root_.breeze.stats.mode
import breeze.util.Index
import io.circe.Decoder
import ada.demos.utility.Utilities
import ada.interface._

trait TestEnsembleHelpers{
    val rnd = scala.util.Random

    def makeGenerator[ModelId, ModelData, ModelAction](idGenerator: Gen[ModelId], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction],
                                                       makeEnsemble: (Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], List[Double], Double) => StackableEnsemble1[ModelId, ModelData, ModelAction, ExpDouble]) = {
    
    val idsRewardsGenerator = for{
        numIds <- Gen.chooseNum(6, 8)
        idsRaw <- Gen.listOfN(numIds, idGenerator)
        rewardValues <- Gen.listOfN(numIds, choose(0.1, 1000.0))
    } yield {(idsRaw.toSet.toList, rewardValues)}

        
    val generator = for{
            modelData <- dataGenerator
            const1 <- actionGenerator
            (ids, rewardValues) <- idsRewardsGenerator
            epsilon <- choose(0.1, 0.7)
        } yield {
            //println(epsilon)
            val modelMap = ids.map{id => (id, new GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble](const1))}.toMap
            val rewardValues2 = rewardValues.map(r => math.abs(r.toDouble)).sorted
            val rewardValues3 = (rewardValues2.take(ids.length - 2).map(r => r/5.0) ++ rewardValues2.drop(ids.length-2).take(1).map(_/1.1) ++ rewardValues2.takeRight(1)).map(r => r/rewardValues2.max)
            //println(rewardValues3.map(n => "f%.2f".format(n)).mkString(" "))
            val ensemble = makeEnsemble(modelMap, rewardValues3 , epsilon)
            (epsilon, ids, ensemble, modelData)
        }
        generator
    }

    def measureActionShares[ModelID, ModelData, ModelAction](data: ModelData, nActions: Int, ensemble: StackableEnsemble1[ModelID, ModelData, ModelAction, ExpDouble]) = {
        //println("measureActionSharesStart")
        val rounds = for {
            i <- (0 until nActions)
        } yield{
            val (action, selectedIds) = ensemble.actWithID(data, Stub())
            //println(selectedIds)
            (action, Utilities.extractSingleValue(selectedIds))
        }
        //println("measureActionSharesEnd")
        rounds
    } 

    def testActionShares[ModelID, ModelData, ModelAction](rounds: IndexedSeq[(ModelAction, ModelID)], epsilon: Double, highRewardId: ModelID, otherIds: List[ModelID]) = {
        val highRewardIdAccordingToepsilon = math.abs(rounds.count(_._2 == highRewardId).toDouble/rounds.length - (1.0-epsilon)) < 0.05
        val shares = otherIds.map(id => rounds.count(t => t._2 == id).toDouble/rounds.length)
        //print(epsilon.toString + " ")
        //println(shares ++ List(rounds.count(_._2 == highRewardId).toDouble/rounds.length))
        val comparisons = shares.flatMap(s => shares.map(t => 1.0 - math.min(s,t)/math.max(s,t)))
        //print(comparisons)
        val otherIdsEquallyLikely = comparisons.sum <= comparisons.length*0.1
        val result = highRewardIdAccordingToepsilon && otherIdsEquallyLikely
        //println((highRewardIdAccordingToepsilon, otherIdsEquallyLikely))
        (highRewardIdAccordingToepsilon, otherIdsEquallyLikely)
    }
}

trait hasMakeEnsembleFn{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double)(implicit modelIdDecoder: Decoder[ModelId]): StackableEnsemble1[ModelId,ModelData,ModelAction,ExpDouble]
}

abstract class TestBasis(label: String, equallyLikely: Boolean) extends Properties(label) 
    with TestEnsembleHelpers
    with hasMakeEnsembleFn {


    def testStackableEnsemble1[ModelID, ModelData, ModelAction](name: String, nActions: Int, idGenerator: Gen[ModelID], dataGenerator: Gen[ModelData], actionGenerator: Gen[ModelAction])(implicit modelIdDecoder: Decoder[ModelID]) = {
        val generator = makeGenerator(idGenerator, dataGenerator, actionGenerator, makeEnsembleFn[ModelID, ModelData, ModelAction])

        property(name + " epsilon model selection") = forAll(generator){
            tuple => {
                val (epsilon, ids, ensemble, modelData) = tuple
                if(ids.length > 2){
                    val rounds = measureActionShares[ModelID, ModelData, ModelAction](modelData, nActions, ensemble)
                    val (accordingToEpsilon, otherIdsEquallyLikely) = testActionShares[ModelID, ModelData, ModelAction](rounds.map(r => (Utilities.extractSingleValue(r._1), r._2)), epsilon, ids.takeRight(1)(0), ids.take(ids.length-1))
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
                    ids.map(id => ensemble.update(Leaf(id), modelData, new Reward(0.5)))
                    ensemble.update(Leaf(ids.head), modelData, new Reward(1.0))
                    val rounds = measureActionShares[ModelID, ModelData, ModelAction](modelData, nActions, ensemble)
                    val (accordingToEpsilon, otherIdsEquallyLikely) = testActionShares[ModelID, ModelData, ModelAction](rounds.map(r => (Utilities.extractSingleValue(r._1), r._2)), epsilon, ids.head, ids.tail)
                    accordingToEpsilon && (otherIdsEquallyLikely )

                } else {
                    true
                }
            }
        }
    }
    
    //these are the actual test executions

    implicit val stringDecoder = Decoder[String]
    implicit val intDecoder = Decoder[Int]

    testStackableEnsemble1("SDD", 10000, arbitrary[String], arbitrary[Double], arbitrary[Double])
    testStackableEnsemble1("SII", 10000, arbitrary[String], arbitrary[Int], arbitrary[Int])
    testStackableEnsemble1("IDIX", 10000, arbitrary[Int], arbitrary[Double], Gen.pick(5, (0 until 1000).toSet))

}

trait MakeEnsembleFnGreedy{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double)(implicit modelIdDecoder: Decoder[ModelId]) = {
        new GreedyEnsemble[ModelId, ModelData, ModelAction, ExpDouble](
                                                                        modelMap,
                                                                        modelMap.keys.zip(rewardValues).map{case(k, r) => (k, new ExpDouble(r))}.toMap,
                                                                        epsilon)
    }
}

trait MakeEnsembleFnGreedySoftmax{
    def makeEnsembleFn[ModelId, ModelData, ModelAction](modelMap: Map[ModelId, GenericStaticModel[ModelId, ModelData, ModelAction, ExpDouble]], rewardValues: List[Double], epsilon: Double)(implicit modelIdDecoder: Decoder[ModelId]) = {
        new GreedySoftmaxEnsemble[ModelId, ModelData, ModelAction, ExpDouble](
                                                                        modelMap,
                                                                        modelMap.keys.zip(rewardValues).map{case(k, r) => (k, new ExpDouble(r))}.toMap,
                                                                        epsilon)
    }
}

class TestEpsilonGreedy extends TestBasis("GreedyEnsemble", true) with MakeEnsembleFnGreedy
class TestEpsilonGreedySoftmax extends TestBasis("GreedySoftmaxEnsemble", false) with MakeEnsembleFnGreedySoftmax