package ada.core.interface

import ada._

import io.circe.Json

trait Model[ModelData, ModelAction] extends Exportable{
    def act(data: ModelData): ModelAction
    //def act[Context](context: Context, data: ModelData): ModelAction
    //def update[ModelID](modelIds: List[ModelID], data: ModelData, reward: Reward): Unit


    def report: String = this.toString
}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def act[Context](context: Context, data: ModelData): ModelAction =
        throw new Exception("Context should not be provided!!")
}

trait DifficultModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def act(data: ModelData): ModelAction = 
        throw new Exception("No Context provided!!")
}


trait ContextualModel[Context, ModelData, ModelAction] 
    extends Model[ModelData, ModelAction]
    with DifficultModel[ModelData, ModelAction]{

    def act(context: Context, data: ModelData): ModelAction
}

trait StackableModel[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}
