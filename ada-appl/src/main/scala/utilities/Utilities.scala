package ada.demos.utility

import ada.core.interface._
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{ListBuffer}

import ada.core.components.distributions.BayesianSampleRegressionContext
import ada.core.ensembles._
import ada.core.models.StaticModel
import plotting.Chart

object Utilities{

    def createContext(highIndexMap: Map[Int, Double], nFeatures: Int, rnd: scala.util.Random): Array[Double] = {
        val context = Array.fill(nFeatures)(rnd.nextGaussian())
        //the context is random except for in the highIndex position, where it is quite high, which results
        //in a higher predicted reward for models with a positive coefficient at that position
        highIndexMap.map{
            case(k, v) => context(k % nFeatures) += v
        }
        context
    }

    def selectAndAverageContext[D, E <: ExportUpdateableContext[Array[Double]]](ensemble: ContextualEnsemble[Int, Array[Double], Unit, D, E], nModels: Int,
                                        highIndexMap: Map[Int, Double], nFeatures: Int, rnd: scala.util.Random,
                                        iter: Int = 100): List[Double] = {
        val context = createContext(highIndexMap, nFeatures, rnd)
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(context, ()))).map(_._2)

        (0 until nModels).map{m => 
            selectedModels.toList.map(s => if(s == m) 1.0 else 0.0).sum / selectedModels.length
        }.toList
    }
    def selectAndAverageStackable[B, C, D <: ExportUpdateable](ensemble: StackableEnsemble1[Int, B, C, D], data: B, nModels: Int, iter: Int = 100): List[Double] = {
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(data, List()))).map(_._2)
        averageSelectedModels(selectedModels, nModels)
    }
    

    def averageSelectedModels(selectedModels: IndexedSeq[List[Int]], nModels: Int): List[Double] = {
        (0 until nModels).map{m => 
            selectedModels.toList.map(s => if(s(0) == m) 1.0 else 0.0).sum / selectedModels.length
        }.toList
    }

    def selectAndAverageDynamic[C, D <: ExportUpdateableContext[Array[Double]]](ensemble: StackableEnsemble2[Int, Array[Double], C, D ], highIndexMap: Map[Int, Double], nFeatures: Int , nModels: Int, rnd: scala.util.Random, iter: Int = 100): List[Double] = {
        val modelData = createContext(highIndexMap, nFeatures, rnd)
        val selectedModels = (for{
            i <- (0 until iter)
        } yield(ensemble.actWithID(modelData, List()))).map(_._2)
        averageSelectedModels(selectedModels, nModels)

    }


    def report(highIndexMap: Map[Int, Double], selections: List[Double], nModels: Int, nIter: Int, nFeatures: Int, nGoodModels: Int, shares: ListBuffer[ListBuffer[Double]]): Unit = {
        val characters = "abcdefghijklmnopqrst"

        var selections2: List[(Double, Int)] = Nil
        //if there are more than 10 models, filter models to those that have good performance
        if( selections.length > 10){
            selections2 = selections.zipWithIndex
                                    .filter{
                                        case(x, i)if( highIndexMap.contains(i% nFeatures) || i < nGoodModels*nFeatures) => true
                                        case _ => false
                                    }
        } else {
            selections2 = selections.zipWithIndex
        }

        println(f"\n\nreport with highIndexMap: $highIndexMap")
        selections2.map{
            case(v, i) => println(f"action ${characters(i % characters.length)}: $v")
        }

        var chart = Chart(1.1, -0.1, 0, nIter)
        (0 until nModels).map{i =>
            chart = chart.plotLine(shares(i).toList, Some(i.toString), characters(i%characters.length).toString())
        }
        println(chart.render())
    }

    def runContext[D, E <: ExportUpdateableContext[Array[Double]]](ensemble: ContextualEnsemble[Int, Array[Double], Unit, D, E], highIndexMaps: List[Map[Int, Double]], 
                    nModels: Int, nIter: Int, nFeatures: Int, nGoodModels: Int,
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
            val (action, id) = ensemble.actWithID(context, List())
            //for the first nGoodModels*nFeatures Models, the reward is the value of the context
            //at the position that is indexed by the remainder of the model id divided by the
            //number of features, for all other models it is 0
            //this means that for those first models, the reward is the value at the feature
            //val setReward = if(id < nFeatures * nGoodModels) context(id % nFeatures) else 0

            val setReward = if((id < nFeatures * nGoodModels) &&  //if the id belongs to a model that sees positive reward
                    ((rnd.nextDouble()*(1+context(id % nFeatures))) > (1-conversionRate(id)))) 5 else 0   // and the customer converts

            ensemble.update(id, context, (),  setReward)

            if(i % 10000 == 0) println(i) 

            i += 1
        }
        println("-----finished loop------")
        shares
    }

    def runStackable[C, D <: ExportUpdateableContext[Array[Double]]](ensemble: StackableEnsemble2[Int, Array[Double], C, D] , highIndexMaps: List[Map[Int, Double]], 
                    nModels: Int, nIter: Int, nFeatures: Int, nGoodModels: Int,
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
            val (action, id) = ensemble.actWithID(context, List())
            //for the first nGoodModels*nFeatures Models, the reward is the value of the context
            //at the position that is indexed by the remainder of the model id divided by the
            //number of features, for all other models it is 0
            //this means that for those first models, the reward is the value at the feature
            //val setReward = if(id < nFeatures * nGoodModels) context(id % nFeatures) else 0

            val setReward = if((id(0) < nFeatures * nGoodModels) &&  //if the id belongs to a model that sees positive reward
                    ((rnd.nextDouble()*(1+context(id(0) % nFeatures))) > (1-conversionRate(id(0))))) 5 else 0   // and the customer converts

            ensemble.update(id, context,  setReward)

            if(i % 10000 == 0) println(i) 

            i += 1
        }
        println("-----finished loop------")
        shares
    }

}

