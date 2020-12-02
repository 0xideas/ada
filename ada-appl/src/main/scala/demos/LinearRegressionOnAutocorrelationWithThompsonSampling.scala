package ada.demos

import ada.core.models.{SimpleLinearRegressionModel, StaticModel}
import ada.core.ensembles.ThompsonSamplingLocalBeta
import ada.core.components.distributions.BetaDistribution
import ada.generators.AutoregressionGenerator
import ada._
import ada.core.components.distributions.{Distribution, BetaDistribution}

import plotting.Chart



object DemoAutocorrelationWithThompsonSampling{
    val models = List(new SimpleLinearRegressionModel[Int](3),
                    new SimpleLinearRegressionModel[Int](10),
                     new SimpleLinearRegressionModel[Int](30))

    //different ensembles share models
    val ensembles = (0 until 3).map{_ => new ThompsonSamplingLocalBeta[Int, Double, Double](
                                                               models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               100, 100)
    }
    
    val ensemble = new ThompsonSamplingLocalBeta[Int, Double, Double](
                                                               ensembles.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               100, 100)


    val generator = new AutoregressionGenerator(3, 0.2)

    
    def run(): Unit = {
        var dataRun: List[Double] = List.fill(15)(1000.0)
        var next = 0.0
        var i = 0.0
        val incr = math.max(1, math.ceil(1000.0/150.0).toInt*2)
        var pars: List[List[String]] = List.fill(models.length)(List())
        var rewards: List[List[String]] = List.fill(models.length)(List())
        var selectedModels: List[String] = List()
        while(i < 1000){
            print("")

            models.zipWithIndex.map{ case(model, i) => model.fitReverse(dataRun.take(model.steps))}
            next = generator.next

            val (action, selectedModel) = ensemble.actWithID(-999, List())
            if (i % incr == 0) {
                pars = pars.zipWithIndex.map{case(p, i) => models(i).toStringM :: p}
                rewards = rewards.zipWithIndex.map{case(r, i) => {
                    val rewardString = (ensemble.modelRewards(i).draw *100).toInt.toString
                    if (rewardString.length == 1) " " + rewardString :: r
                    else rewardString :: r
                    }
                }
                selectedModels = selectedModel.map(_.toString) ::: selectedModels            
            }

            //ensemble.update(selectedModel, action, next )
            dataRun = next :: dataRun
            ensemble.update(selectedModel, action, next)
            //ensemble.updateAll(-999, next)

            i = i + 1.0
        }
        dataRun = dataRun.take(1000).reverse
        println("\n" + Chart(dataRun.max.toInt, dataRun.min.toInt, 0, dataRun.length).plotLine(dataRun, Some("M")).render())
        println("coefficients")
        println(pars.map(p => p.reverse.mkString("")).mkString("\n"))
        println("reward    //models with an id below (goodModels * nFeatures) and where the remainder of id divided//by the s")
        println(rewards.map(r => r.reverse.mkString("")).mkString("\n"))
        println("selected model")
        println(" " + selectedModels.reverse.mkString(" "))
        println("model frequencies")
        println(selectedModels.reverse.groupBy(identity).view.map{case(k,v) => (k, v.size)}.toMap.toList.sortWith(_._2 > _._2).map{case(a,b) => s"Model $a -> $b"}.mkString("\n"))

    }

}
