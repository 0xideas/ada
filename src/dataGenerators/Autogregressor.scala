package epsilon

class Regression{
	private var m = 0.0
	private var b = 0.0
	private val eta = 0.0000000000001
	def listMean(l: List[Double]): Double = l.sum/l.length
	def correlation(a: List[Double], b: List[Double]): Double = {
		val aMean = listMean(a)
		val bMean = listMean(b)
		listMean(a.zip(b).map{case(aa, bb) => (aa-aMean)*(bb-bMean)})/(standardDeviation(a)*standardDeviation(b) + eta)
	}
	def standardDeviation(a: List[Double]): Double = {
		val mean = listMean(a)
		math.sqrt(listMean(a.map(aa => math.pow(aa-mean, 2))))
	}
	def fitReverse(y: List[Double]): Regression = {
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
}


class Autoregressor(steps: Int, var vol:Double){
    private val rnd = new scala.util.Random
    private var values = rnd.nextDouble+999.4999 :: Array.fill(steps+1)(1000.0).toList
    private var regressor = new Regression()
    private var lastVol = (rnd.nextGaussian)/50
    def next(): Double = {
        val nextRegressed = regressor.fitReverse(values.take(steps)).predict(steps.toDouble-0.6)
    	val volatility:Double = ((rnd.nextGaussian/50)+ lastVol*0.5 + 0.0003)
    	lastVol = volatility
 		val nextVal = nextRegressed * (1.0+volatility*vol)
        values = nextVal :: values
        nextVal
    }
}

