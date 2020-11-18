package ada.demos

import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{ListBuffer}

import ada.core.components.distributions.BayesianSampleRegressionContext
import ada.core.ensembles._
import ada.core.models.StaticModel


import plotting.Chart

object DemoBayesianRegressionContextSparseMany{
    //parameters for the demo
    val nIter = 1000 * 20
    val nFeatures = 5
    val nModels = 3
    val nGoodModels = 2

    val conversionRate = Map(
        0 -> 0.013,
        1 -> 0.017,
        2 -> 0.022
    )

    //highIndexMaps are the perturbations applied to the random context when taking a snapshot
    //this way the effect of new contexts on the ensemble can be studied

    //val highIndexMaps: List[Map[Int, Int]] = (0 until nFeatures).toList.map(v => Map(v -> 4))
    val highIndexMaps: List[Map[Int, Double]] = List(
        Map(0 -> 1.2),
        Map(1 -> 1.2),
        Map(2 -> 1.2),
        Map(0 -> 1.2, 1 -> 1.2, 2 -> 1.2),
        Map()
    )

    val rnd = scala.util.Random

    //initialisation of the ensemble
    val models = (0 until nModels).map(x => new StaticModel(x.toDouble))
    val contexts = (0 until nModels).map(x => new BayesianSampleRegressionContext(nFeatures, 0.15, 1.0, 1.0))
    val ensemble = new ThompsonSamplingLocalWithContext[Int, Array[Double], Unit, Double,  BayesianSampleRegressionContext](
        (0 until nModels).zip(models).toMap,
        MutableMap((0 until nModels).zip(contexts):_*),
        (action1, action2) => math.exp(action1 - action2)
    )


    def getAverages(highIndexMap: Map[Int, Double], iter: Int = 100): List[Double] = {
        //given a particular state of the ensemble, act 100 times and measure the frequency of each action
        val context = Array.fill(nFeatures)(rnd.nextGaussian())
        //the context is random except for in the highIndex position, where it is quite high, which results
        //in a higher predicted reward for models with a positive coefficient at that position
        highIndexMap.map{
            case(k, v) => context(k % nFeatures) += v
        }
        val selected = ListBuffer.fill(nModels)(ListBuffer.empty[Double])
        var j = 0
        while( j < iter) {
            val (action, id) = ensemble.actWithID(context, ())
            //one hot encode which action was taken, so to speak
            selected.zipWithIndex.map{case(a, i) => selected(i) += (if(i == id) 1.0 else 0.0)}
            j += 1
        }
        selected.map(s => s.sum/s.length).toList
    }

    def report(highIndexMap: Map[Int, Double], shares: ListBuffer[ListBuffer[Double]]): Unit = {
        val characters = "abcdefghijklmnopqrst"

        val selections = getAverages(highIndexMap, 10000)
        var selections2: List[(Double, Int)] = Nil
        //if there are more than 10 models, filter models to those that have good performance
        if( selections.length > 10){
            selections2 = selections.zipWithIndex
                                    .filter{
                                        case(x, i)if( highIndexMap.contains(i% nFeatures) && i < nGoodModels*nFeatures) => true
                                        case _ => false
                                    }
        } else {
            selections2 = selections.zipWithIndex
        }

        println(f"\n\nreport with highIndexMap: $highIndexMap")
        selections2.map{
            case(v, i) => println(f"action ${characters(i)}: $v")
        }

        var chart = Chart(1.1, -0.1, 0, nIter)
        (0 until nModels).map{i =>
            chart = chart.plotLine(shares(i).toList, Some(i.toString), characters(i%characters.length).toString())
        }
        println(chart.render())
    }



    def run(): Unit = {
        println("started run")
        val shares = ListBuffer.fill(highIndexMaps.length)(ListBuffer.fill(nModels)(ListBuffer.empty[Double]))

        var i = 0
        while(i < nIter){

            //take 100 snapshots in total for charting
            if(i % scala.math.max(1, (nIter / 100).toInt)  == 0){
                highIndexMaps.zipWithIndex.map{
                    case(highIndexMap, f) =>  {
                        var selections = getAverages(highIndexMap, 1000)
                        shares(f).zipWithIndex.map{case(s,j) => s += selections(j)}  
                    }
              }

            }

            val context = Array.fill(nFeatures)(rnd.nextGaussian())
            val (action, id) = ensemble.actWithID(context, ())
            //for the first nGoodModels*nFeatures Models, the reward is the value of the context
            //at the position that is indexed by the remainder of the model id divided by the
            //number of features, for all other models it is 0
            //this means that for those first models, the reward is the value at the feature
            val setReward = if((id < nFeatures * nGoodModels) &&  //if the id belongs to a model that sees positive reward
                               ((rnd.nextDouble()*(1+context(id % nFeatures))) > (1-conversionRate(id)))) 5 else 0   // and the customer converts

            ensemble.update(id, context,  setReward)

            if(i % 10000 == 0) println(i) 

            i += 1
        }
        println("-----finished loop------")

        highIndexMaps.zipWithIndex.map{
            case(highIndexMap, f) => {
                report(highIndexMap, shares(f))
            }
        }

    }
}
