package epsilon.core.models

import epsilon.core.interface.ModelNoContext

class SimpleLinearRegression(private var m: Double = 0.0, private var b: Double = 0.0){
	private val eta = 0.0000000000001
    def listMean(l: List[Double]): Double = l.sum/l.length
    
	def correlation(a: List[Double], b: List[Double]): Double = {
		val aMean = listMean(a)
		val bMean = listMean(b)
		listMean(a.zip(b).map{
			case(aa, bb) => (aa-aMean)*(bb-bMean)})/(standardDeviation(a)*standardDeviation(b)+eta)
    }
    
	def standardDeviation(a: List[Double]): Double = {
		val mean = listMean(a)
		math.sqrt(listMean(a.map(aa => math.pow(aa-mean, 2))))
    }
    
	def fitReverse(y: List[Double]): SimpleLinearRegression = {
		val x = (y.length-1 to 0 by -1).toList.map(_.toDouble)
		m = correlation(x, y)*(standardDeviation(y)/standardDeviation(x)+eta)
		b = listMean(y) - m*listMean(x)
		//println(s"$m*x + $b")
		this
    }
    
	def predict(x: Double): Double = {
		x*m + b
    }
    
	def getM: Double = m
	
	override def toString: String = 
		if(b > 0) s"$m*x + $b" else {val nb = -b; s"$m*x - $nb"}
	def toStringShort: String = {
		val mm= m.toInt
		if(b > 0) {val bb = b.toInt; s"$mm*x+$bb"} 
		else {val nb = -b.toInt; s"$mm*x-$nb"}
	}
	def toStringM:String =  if(m.toInt >= 0) " " + m.toInt.toString else m.toInt.toString 

}

class SimpleAutoRegressionModel(val steps: Int,
								private var m: Double = 0.0,
								private var b: Double = 0.0)
    extends SimpleLinearRegression(m, b)
    with ModelNoContext[Double, Double] {

	def act(x: Double) = predict(steps)
	
}