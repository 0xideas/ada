
class DummyModel(possibleValues: List[Int]) extends Model {
    private val rnd = new scala.util.Random

    def act(data: ModelData): ModelAction = {
        val i = math.abs(rnd.nextInt)
        data.min > possibleValues(i % possibleValues.size)
    }

    override def toString: String = "$Model: " + possibleValues.toString() + "$"
} 

object Debugging {
    def printEpsilon(s: String): Unit =
        println(s)
    val m1 = new DummyModel(List(1, 2, 3))
    val m2 = new DummyModel(List(0, 1))
    val m3 = new DummyModel(List(3))
    val ensemble = new EpsilonEnsembleLocal(0.4, List(m1, m2, m3), (aggReward, reward) => (aggReward * 0.8) + reward*0.2 )  
}
