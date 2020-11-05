package epsilon.generators

import epsilon.core.models.SimpleLinearRegression
import java.sql.Time

trait Generator[ModelAction]{
	def next: ModelAction
}

trait TimeseriesGenerator extends Generator[Double]{
	def next: Double
}

class AutoregressionGenerator(steps: Int, var vol:Double) extends TimeseriesGenerator{
    private val rnd = new scala.util.Random
    private var values = rnd.nextDouble()+999.4999 :: Array.fill(steps+1)(1000.0).toList
    private val regressor = new SimpleLinearRegression()
    private var lastVol = (rnd.nextGaussian())/50
    def next: Double = {
        val nextRegressed = regressor.fitReverse(values.take(steps)).predict(steps.toDouble-0.6)
    	val volatility:Double = ((rnd.nextGaussian()/50)+ lastVol*0.5 + 0.0003)
    	lastVol = volatility
 		val nextVal = nextRegressed * (1.0+volatility*vol)
        values = nextVal :: values
        nextVal
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

