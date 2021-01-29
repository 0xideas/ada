package ada.core.interface

import ada._

import io.circe.Json

trait Model[ModelData, ModelAction] extends Exportable{
    def report: String = this.toString
}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def update(data: ModelData, action: ModelAction): Unit
    def act(data: ModelData): ModelAction
}

trait ContextualModel[ModelID, Context, ModelData, ModelAction] 
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], context: Context, data: ModelData, reward: Reward): Unit
    //def update(context: Context, data: ModelData, reward: Reward): Unit
    def update(modelIds: List[ModelID], context: Context, data: ModelData, action: ModelAction): Unit
    def actWithID(context: Context, data: ModelData, modelIds: List[ModelID]): (ModelAction, List[ModelID])
}

trait StackableModel[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, action: ModelAction): Unit
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID])
}


trait ContextualModelPassive[ModelID, Context, ModelData, ModelAction, AggregateReward]
    extends ContextualModel[ModelID, Context, ModelData, ModelAction]{
        def update(context: Context, data: ModelData, optimalAction: ModelAction): Unit
        def evaluate(action: ModelAction,optimalAction: ModelAction): ada.Reward
        //def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
        def updateAll(modelIds: List[ModelID], context: Context,  data: ModelData, optimalAction: ModelAction): Unit
}

trait StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModel[ModelID, ModelData, ModelAction]{
        def update(data: ModelData, optimalAction: ModelAction): Unit
        def evaluate(action: ModelAction,optimalAction: ModelAction): ada.Reward
        //def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit
        def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit
}



trait StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]{
    def update(modelIds: List[ModelID], data: ModelData, action: ModelAction): Unit
    def updateAll(modelIds: List[ModelID], data: ModelData, optimalAction: ModelAction): Unit = {
            update(data, optimalAction)
        }
}



trait InertModel[ModelID, ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, action: ModelAction): Unit =  ()
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit =  ()
    def update(modelIds: List[ModelID],reward: Reward): Unit =  ()
    def update(data: ModelData, reward: Reward): Unit =  ()
    def update(reward: Reward): Unit =  ()
    def update(data: ModelData, action: ModelAction): Unit = ()
    def evaluate(action: ModelAction, optimalAction: ModelAction): ada.Reward = new Reward(0.0)

}
trait InertModelContext[ModelID, Context, ModelData, ModelAction] extends InertModel[ModelID, ModelData, ModelAction]{

    def update(modelIds: List[ModelID],context: Context,data: ModelData,action: ModelAction): Unit = ()
    def update(modelIds: List[ModelID],context: Context,data: ModelData,reward: ada.Reward): Unit = ()
}
