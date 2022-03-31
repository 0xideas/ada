package ada.demos.utility

import ada.interface._
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{ListBuffer}

import ada.components.distributions.BayesianSampleRegressionDistribution
import ada.ensembles._
import ada.models.StaticModel
import plotting.Chart
import ada.`package`.Reward
import ada.interface.Tree

object Utilities{

    def extractSingleValue[V](v: Tree[V]): V = {
        v match {
            case Leaf(value) => value
            case Twig(value, _) => value
            case _ => throw new Exception("Can only extract single value from Leaf or Twig")
        }

    }

    def createContext(highIndexMap: Map[Int, Double], nFeatures: Int, rnd: scala.util.Random): Array[Double] = {
        val context = Array.fill(nFeatures)(rnd.nextGaussian())
        //the context is random except for in the highIndex position, where it is quite high, which results
        //in a higher predicted reward for models with a positive coefficient at that position
        highIndexMap.map{
            case(k, v) => context(k % nFeatures) += v
        }
        context
    }

    def selectStackable[B, C, D <: Updateable](
        ensemble: StackableEnsemble1[Int, B, C, D],
        data: B,
        nModels: Int,
        iter: Int = 100): IndexedSeq[ada.interface.Tree[Int]] = {
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(data, Stub()))).map(_._2)
        selectedModels
    }

    def averageSelectedModels(selectedModels: IndexedSeq[Tree[Int]], nModels: Int): List[Double] = {
        (0 until nModels).map{m => 
            selectedModels.toList.map(s => if(extractSingleValue(s) == m) 1.0 else 0.0).sum / selectedModels.length
        }.toList
    }

    def selectAndAverageStackable[B, C, D <: Updateable](
        ensemble: StackableEnsemble1[Int, B, C, D],
        data: B,
        nModels: Int,
        iter: Int = 100): List[Double] = {
        val selectedModels = selectStackable[B,C,D](ensemble, data, nModels, iter)
        averageSelectedModels(selectedModels, nModels)
    }


    def averageSelectedModelsLevel2(selectedModels: IndexedSeq[Tree[Int]], nModelsLevel1: Int, nModelsLevel2: Int): List[Double] = {
        (0 until nModelsLevel1*nModelsLevel2).map{m => 
            selectedModels.toList.map(s => {val s2 = Tree.concatenateTree(s); if( s2(0) * nModelsLevel1 + s2(1) == m) 1.0 else 0.0}).sum / selectedModels.length
        }.toList
    }


    def selectAndAverageDynamic[C, D <: UpdateableContext[Array[Double]]](ensemble: StackableEnsemble2[Int, Array[Double], C, D ], highIndexMap: Map[Int, Double], nFeatures: Int , nModels: Int, rnd: scala.util.Random, iter: Int = 100): List[Double] = {
        val modelData = createContext(highIndexMap, nFeatures, rnd)
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(modelData, Stub()))).map(_._2)
        averageSelectedModels(selectedModels, nModels)

    }

    def selectAndAverageContext[D, E <: UpdateableContext[Array[Double]]](
        ensemble: ContextualEnsemble[Int, Array[Double], Unit, D, E],
        nModels: Int,
        highIndexMap: Map[Int, Double],
        nFeatures: Int,
        rnd: scala.util.Random,
        iter: Int = 100): List[Double] = {
        val context = createContext(highIndexMap, nFeatures, rnd)
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(context, (), Stub()))).map(_._2)
        averageSelectedModels(selectedModels, nModels)

    }

    def runContext[D, E <: UpdateableContext[Array[Double]]](
        ensemble: ContextualEnsemble[Int, Array[Double], Unit, D, E],
        highIndexMaps: List[Map[Int, Double]], 
        nModels: Int,
        nIter: Int, 
        nFeatures: Int,
        nGoodModels: Int,
        rnd: scala.util.Random,
        conversionRate: Map[Int, Double]): ListBuffer[ListBuffer[ListBuffer[Double]]] = {

        println("started run")
        val shares = ListBuffer.fill(highIndexMaps.length)(ListBuffer.fill(nModels)(ListBuffer.empty[Double]))

        var i = 0
        while(i < nIter){

            //take 100 snapshots in total for charting
            if(i % scala.math.max(1, (nIter / 100).toInt)  == 0){
                highIndexMaps.zipWithIndex.map{
                    case(highIndexMap, f) =>  {
                        var selections = Utilities.selectAndAverageContext[D, E](ensemble, nModels, highIndexMap, nFeatures, rnd, 100)
                        shares(f).zipWithIndex.map{case(s,j) => s += selections(j)}  
                    }
              }

            }

            val context = Array.fill(nFeatures)(rnd.nextGaussian())
            val (action, ids) = ensemble.actWithID(context, (), Stub())
            val id = extractSingleValue(ids)
            //for the first nGoodModels*nFeatures Models, the reward is the value of the context
            //at the position that is indexed by the remainder of the model id divided by the
            //number of features, for all other models it is 0
            //this means that for those first models, the reward is the value at the feature
            //val setReward = if(id < nFeatures * nGoodModels) context(id % nFeatures) else 0

            val setReward = if((id < nFeatures * nGoodModels) &&  //if the id belongs to a model that sees positive reward
                    ((rnd.nextDouble()*(1+context(id % nFeatures))) > (1-conversionRate(id)))) 5 else 0   // and the customer converts

            ensemble.update(ids, context, (),  new Reward(setReward))

            if(i % 10000 == 0) println(i) 

            i += 1
        }
        println("-----finished loop------")
        shares
    }
    def runStackable[C, D <: UpdateableContext[Array[Double]]](
        ensemble: StackableEnsemble2[Int, Array[Double], C, D],
        highIndexMaps: List[Map[Int, Double]], 
        nModels: Int,
        nIter: Int,
        nFeatures: Int,
        nGoodModels: Int,
        rnd: scala.util.Random,
        conversionRate: Map[Int, Double]): ListBuffer[ListBuffer[ListBuffer[Double]]] = {

        println("started run")
        val shares = ListBuffer.fill(highIndexMaps.length)(ListBuffer.fill(nModels)(ListBuffer.empty[Double]))

        var i = 0
        while(i < nIter){

            //take 100 snapshots in total for charting
            if(i % scala.math.max(1, (nIter / 100).toInt)  == 0){
                highIndexMaps.zipWithIndex.map{
                    case(highIndexMap, f) =>  {
                        var selections = Utilities.selectAndAverageDynamic[C, D](ensemble, highIndexMap, nFeatures, nModels, rnd, 100)
                        shares(f).zipWithIndex.map{case(s,j) => s += selections(j)}  
                    }
              }
            }
            val context = Array.fill(nFeatures)(rnd.nextGaussian())
            val (action, id) = ensemble.actWithID(context, Stub())
            //for the first nGoodModels*nFeatures Models, the reward is the value of the context
            //at the position that is indexed by the remainder of the model id divided by the
            //number of features, for all other models it is 0
            //this means that for those first models, the reward is the value at the feature
            //val setReward = if(id < nFeatures * nGoodModels) context(id % nFeatures) else 0

            val setReward = if((extractSingleValue(id) < nFeatures * nGoodModels) &&  //if the id belongs to a model that sees positive reward
                    ((rnd.nextDouble()*(1+context(extractSingleValue(id) % nFeatures))) > (1-conversionRate(extractSingleValue(id))))) 5 else 0   // and the customer converts

            ensemble.update(id, context,  new Reward(setReward))

            if(i % 10000 == 0) println(i) 

            i += 1
        }
        println("-----finished loop------")
        shares
    }
    
}

