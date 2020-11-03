package epsilon.models
import epsilon.interfaces.NoContextModel

class DummyModel(value: Double) extends NoContextModel[Double, Double]{

    def act(data: Double): Double = value

    def update(value: Double): DummyModel = new DummyModel(value)

    override def toString: String = "$Model: " + value.toString() + "$"
} 

class GenericDummyModel[ModelData, ModelAction](value: ModelAction) extends NoContextModel[ModelData, ModelAction]{

    def act(data: ModelData): ModelAction = value

    def update(value: ModelAction): GenericDummyModel[ModelData, ModelAction] = new GenericDummyModel[ModelData, ModelAction](value)

    override def toString: String = "$Model: " + value.toString() + "$"
} 