package ada.core.interface

import io.circe.Json

trait Model[ModelData, ModelAction] extends Exportable{
    def act(data: ModelData): ModelAction
    //def act[Context](context: Context, data: ModelData): ModelAction

    def report: String = this.toString

}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def act[Context](context: Context, data: ModelData): ModelAction =
        throw new Exception("Context should not be provided!!")
}  

trait ContextualModel[Context, ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    override def act(data: ModelData): ModelAction = 
        throw new Exception("No Context provided!!")
    def act(context: Context, data: ModelData): ModelAction
}

