package epsilon.core.interface

trait Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction
    //def act[Context](context: Context, data: ModelData): ModelAction

    def report: String = this.toString
}

trait ModelNoContext[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def act[Context](context: Context, data: ModelData): ModelAction =
        throw new Exception("Context should not be provided!!")
}  

trait ModelWithContext[Context, ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    override def act(data: ModelData): ModelAction = 
        throw new Exception("No Context provided!!")
    def act(context: Context, data: ModelData): ModelAction
}

