package epsilon

import scala.collection.mutable.ListBuffer
import epsilon.SimpleAutoRegressionModel
import epsilon.EpsilonEnsembleLearnerLocal
import epsilon.AutoregressionGenerator

object DemoAutocorrelation{
    val models = List(new SimpleAutoRegressionModel(2),
                    new SimpleAutoRegressionModel(5),
                    new SimpleAutoRegressionModel(10),
                    new SimpleAutoRegressionModel(15),
                     new SimpleAutoRegressionModel(100))

    val evaluationFn = (action: Double, correctAction: Double) => math.max(1.0, 10-math.pow(action-correctAction, 2))
    val ensemble = EpsilonEnsembleLearnerLocal[Double, Double, Int](0.0,
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
        println("\n" + plotLine(dataRun))
        println("\n" + plotLine(dataRun.take(150)))
        println("\n" + plotLine(dataRun.drop(150).take(150), 150))

        println("")

    }

    def plotLine(data: List[Double], xAxisStart: Int = 0, width: Int = 150, height: Int = 40): String = {
        val max = data.max
        val min = data.min

        def bucket(x: Double): Int = (((x-min)/(max -min))*(height-0.001)).toInt

        val chars = ListBuffer.fill(height-1)(ListBuffer("|") :++ ListBuffer.fill(width-1)(" ")) :+ (ListBuffer("|") :++ ListBuffer.fill(width-1)("_"))
        val incr = math.max(1, math.ceil(data.length.toDouble/(width.toDouble)).toInt)

        var plotMax = 0
        data.zipWithIndex
            .filter{case(x, i) => i%incr == 0}
            .map(_._1)
            .zipWithIndex
            .tail
            .map{ case(x, i) =>{plotMax = math.max(plotMax, i); chars(bucket(x))(i) = "-" }}

        max.toInt.toString.toList.zipWithIndex.map{ case(c, i) => chars(0)(i) = c.toString}
        (min.toInt.toString + "/" + xAxisStart.toInt).toList.zipWithIndex.map{ case(c, i) => chars(height-1)(i) = c.toString}
        (data.length + xAxisStart).toString.toList.zipWithIndex.map{ case(c, i) => chars(height-1)(plotMax- data.length.toString.length+i) = c.toString}

        (plotMax until width).map(i => chars(height-1)(i) = " ")
        chars.map(row => row.mkString("")).mkString("\n")

    }

}
