package epsilon

import plotting.Chart

import epsilon.SimpleAutoRegressionModel
import epsilon.EpsilonEnsembleGreedySoftmaxLocal
import epsilon.AutoregressionGenerator

object DemoAutocorrelation{
    val models = List(new SimpleAutoRegressionModel(2),
                    new SimpleAutoRegressionModel(5),
                    new SimpleAutoRegressionModel(10),
                    new SimpleAutoRegressionModel(15),
                     new SimpleAutoRegressionModel(100))

    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = EpsilonEnsembleGreedySoftmaxLocal[Int, Double, Double](0.0,
                                                               models.zipWithIndex.toMap.map{case(k,v) => (v, k)},
                                                               (aggRew, rew) => rew,
                                                               evaluationFn)

    val generator = new AutoregressionGenerator(2, 0.1)

    
    def run(): Unit = {
        var dataRun: List[Double] = List.fill(15)(1000.0)
        var next = 0.0
        var i = 0.0
        while(i < 1000){
            print("")
            models.zipWithIndex.map{ case(model, i) => model.fitReverse(dataRun.take(model.steps))}
            next = generator.next
            val (action, selectedModel) = ensemble.act(-999)
            //ensemble.update(selectedModel, action, next )
            ensemble.learn(-999, next, aw => true)
            i = i + 1.0
            dataRun = next :: dataRun
            print(selectedModel.toString + "-")

        }
        dataRun = dataRun.take(1000).reverse
        println("\n" + Chart(dataRun.max.toInt, dataRun.min.toInt, 0, dataRun.length).plotLine(dataRun, "M").render())
        val first150 = dataRun.take(150)
        println("\n" + Chart(first150.max.toInt, first150.min.toInt, 0, first150.length).plotLine(first150, "F").render())

        //println("\n" + plotLine(dataRun))
        //println("\n" + plotLine(dataRun.take(150)))
        //println("\n" + plotLine(dataRun.drop(150).take(150), 150))

        println("")

    }

}
