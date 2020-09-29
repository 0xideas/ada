package models.models
import epsilon.interfaces.Model

class DummyModel(possibleValues: List[Int]) extends Model[List[Int], Boolean]{
    private val rnd = new scala.util.Random

    def act(data: List[Int]): Boolean = {
        val i = math.abs(rnd.nextInt)
        data.min > possibleValues(i % possibleValues.size)
    }
    override def toString: String = "$Model: " + possibleValues.toString() + "$"
} 