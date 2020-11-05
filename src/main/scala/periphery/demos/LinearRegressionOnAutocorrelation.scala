package epsilon.demos

import epsilon.core.models.SimpleAutoRegressionModel
import epsilon.core.ensembles.EpsilonEnsembleGreedySoftmaxLocal
import epsilon.generators.AutoregressionGenerator

import scala.collection.mutable.{Map => MutableMap}

import plotting.Chart


object DemoAutocorrelation{
    val models = List(new SimpleAutoRegressionModel(3),
                    new SimpleAutoRegressionModel(10),
                     new SimpleAutoRegressionModel(30))

    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = new EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double, Double](
        models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
        MutableMap(models.zipWithIndex.toSeq.map{case(k,v) => (v, 1.0)}:_*),
        (aggRew:Double) => aggRew,
        0.0,
        evaluationFn,
        (aggRew:Double, rew:Double) => rew)

    val generator = new AutoregressionGenerator(10, 0.2)

    
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

            val (action, selectedModel) = ensemble.actWithID(-999)

            if (i % incr == 0) {
                pars = pars.zipWithIndex.map{case(p, i) => models(i).toStringM :: p}
                rewards = rewards.zipWithIndex.map{case(r, i) => {
                    val rewardString = ensemble.getModelRewards(i).toInt.toString
                    if (rewardString.length == 1) " " + rewardString :: r
                    else rewardString :: r
                    }
                }
                selectedModels = selectedModel.toString :: selectedModels            
            }

            //ensemble.update(selectedModel, action, next )
            dataRun = next :: dataRun
            ensemble.updateAll(-999, next)

            i = i + 1.0
        }
        dataRun = dataRun.take(1000).reverse
        println("\n" + Chart(dataRun.max.toInt, dataRun.min.toInt, 0, dataRun.length).plotLine(dataRun, "M").render())
        println("coefficients")
        println(pars.map(p => p.reverse.mkString("")).mkString("\n"))
        println("rewards")
        println(rewards.map(r => r.reverse.mkString("")).mkString("\n"))
        println("selected model")
        println(" " + selectedModels.reverse.mkString(" "))
        println("model frequencies")
        println(selectedModels.reverse.groupBy(identity).view.map{case(k,v) => (k, v.size)}.toMap.toList.sortWith(_._2 > _._2).map{case(a,b) => s"Model $a -> $b"}.mkString("\n"))

    }

}
