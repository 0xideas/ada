package epsilon.demos

import epsilon.models.{SimpleAutoRegressionModel, DummyModel}
import epsilon.ensembles.{EpsilonEnsembleThompsonSamplingLocal, BetaDistribution}
import epsilon.generators.AutoregressionGenerator

import plotting.Chart



object DemoAutocorrelationWithThompsonSampling{
    val models = List(new SimpleAutoRegressionModel(3),
                    new SimpleAutoRegressionModel(10),
                     new SimpleAutoRegressionModel(30))

    val evaluationFn = (action: Double, correctAction: Double) => math.max(0.0, (20.0-math.pow(action-correctAction, 2))/20)
    val ensemble = EpsilonEnsembleThompsonSamplingLocal[Int, Double, Double](
                                                               models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               (prior, rew) => {prior.update(rew); prior},
                                                               evaluationFn,
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

            val (action, selectedModel) = ensemble.actWithID(-999)
            if (i % incr == 0) {
                pars = pars.zipWithIndex.map{case(p, i) => models(i).toStringM :: p}
                rewards = rewards.zipWithIndex.map{case(r, i) => {
                    val rewardString = (ensemble.getModelRewardsMap(i).draw *100).toInt.toString
                    if (rewardString.length == 1) " " + rewardString :: r
                    else rewardString :: r
                    }
                }
                selectedModels = selectedModel.toString :: selectedModels            
            }

            //ensemble.update(selectedModel, action, next )
            dataRun = next :: dataRun
            //ensemble.update(selectedModel, action, next)
            ensemble.learn(-999, next, aw => true)

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
