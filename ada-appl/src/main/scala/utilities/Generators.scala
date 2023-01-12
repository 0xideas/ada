package ada.generators

//import ada.core.models.SimpleLinearRegression
import java.sql.Time
import plotting.Chart
import scala.collection.mutable.ListBuffer

trait Generator[ModelAction]{
	def next: ModelAction
}

trait TimeseriesGenerator extends Generator[Double]{
	def next: Double
}



class AutoregressionGenerator extends TimeseriesGenerator{
    private val rnd = new scala.util.Random
    private var value = rnd.nextDouble()
    private var lastDifference = 0.0

    def next: Double = {
        val difference = (rnd.nextDouble()-0.5)*0.3
        value += difference + lastDifference
        lastDifference = difference
        value
    }
}

class SineGenerator(amplitude: Double = 1.0, resolution: Double = 100) extends TimeseriesGenerator {
	private var state = 0.0
	def next: Double = {
		state = state + (2*math.Pi)/resolution
		math.sin(state)*amplitude
	} 
}

class ConstantGenerator[ModelAction](const: ModelAction) extends  Generator[ModelAction]{
    def next: ModelAction = const
} 


class ShowGeneratorExample(generator: Generator[Double], nIter: Int = 1000){
    val values = (0 until nIter).map(i => generator.next)
    val chart = Chart(1+values.max, -1+values.min, 0, nIter)
    chart.plotLine(values.toList, None, "+")
    println(chart.render())
}