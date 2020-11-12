package epsilon.core.models

import epsilon.core.interface.ModelNoContext



class GenericStaticModel[ModelData, ModelAction](value: ModelAction)
    extends ModelNoContext[ModelData, ModelAction]{

    def act(data: ModelData): ModelAction = value

    def update(value: ModelAction): GenericStaticModel[ModelData, ModelAction] = 
        new GenericStaticModel[ModelData, ModelAction](value)

    override def toString: String = "$Model: " + value.toString() + "$"
} 


class StaticModel(value: Double) extends GenericStaticModel[Unit, Double](value)