package epsilon.models
import epsilon.interfaces.Model

class DummyModel(value: Double) extends Model[Double, Double]{

    def act(data: Double): Double = value

    def update(value: Double): DummyModel = new DummyModel(value)

    override def toString: String = "$Model: " + value.toString() + "$"
} 