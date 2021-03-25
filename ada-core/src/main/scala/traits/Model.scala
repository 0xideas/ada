package ada.interface

import ada._


trait Model[ModelData, ModelAction]{
    def report: String = this.toString
}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def update(data: ModelData, action:  LTree[ModelAction]): Unit
    def act(data: ModelData): LTree[ModelAction]
}

trait ContextualModel[ModelID, Context, ModelData, ModelAction] 
    extends Model[ModelData, ModelAction]{
    def update(modelIds: LTree[ModelID], context: Context, data: ModelData, reward: Reward): Unit
    //def update(context: Context, data: ModelData, reward: Reward): Unit
    def update(modelIds: LTree[ModelID], context: Context, data: ModelData, action:  LTree[ModelAction]): Unit
    def actWithID(context: Context, data: ModelData, modelIds: LTree[ModelID]): ( LTree[ModelAction], LTree[ModelID])
}

trait StackableModel[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: LTree[ModelID], data: ModelData, action:  LTree[ModelAction]): Unit
    def update(modelIds: LTree[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: LTree[ModelID]): (LTree[ModelAction], LTree[ModelID])
}


trait ContextualModelPassive[ModelID, Context, ModelData, ModelAction, AggregateReward]
    extends ContextualModel[ModelID, Context, ModelData, ModelAction]{
        def update(context: Context, data: ModelData, optimalAction:  LTree[ModelAction]): Unit
        def evaluate(action: LTree[ModelAction], optimalAction:  LTree[ModelAction]): ada.Reward
        //def update(modelIds: LTree[ModelID], data: ModelData, reward: Reward): Unit
        def updateAll(modelIds: LTree[ModelID], context: Context,  data: ModelData, optimalAction:  LTree[ModelAction]): Unit
}

trait StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModel[ModelID, ModelData, ModelAction]{
        def update(data: ModelData, optimalAction:  LTree[ModelAction]): Unit
        def evaluate(action:  LTree[ModelAction],optimalAction:  LTree[ModelAction]): ada.Reward
        //def update(modelIds: LTree[ModelID], data: ModelData, reward: Reward): Unit
        def updateAll(modelIds: LTree[ModelID], data: ModelData, optimalAction:  LTree[ModelAction]): Unit
}



trait StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]{
    def update(modelIds: LTree[ModelID], data: ModelData, action:  LTree[ModelAction]): Unit
    def updateAll(modelIds: LTree[ModelID], data: ModelData, optimalAction:  LTree[ModelAction]): Unit = {
            update(data, optimalAction)
        }
}



trait InertModel[ModelID, ModelData, ModelAction]{
    def update(modelIds: LTree[ModelID], data: ModelData, action:  LTree[ModelAction]): Unit =  ()
    def update(modelIds: LTree[ModelID], data: ModelData, reward: Reward): Unit =  ()
    def update(modelIds: LTree[ModelID],reward: Reward): Unit =  ()
    def update(data: ModelData, reward: Reward): Unit =  ()
    def update(reward: Reward): Unit =  ()
    def update(data: ModelData, action:  LTree[ModelAction]): Unit = ()
    def evaluate(action:  LTree[ModelAction], optimalAction: LTree[ModelAction]): ada.Reward = new Reward(0.0)
}

trait InertModelContext[ModelID, Context, ModelData, ModelAction] extends InertModel[ModelID, ModelData, ModelAction]{

    def update(modelIds: LTree[ModelID], context: Context, data: ModelData, reward: Reward): Unit = ()
    //def update(context: Context, data: ModelData, reward: Reward): Unit
    def update(modelIds: LTree[ModelID], context: Context, data: ModelData, action:  LTree[ModelAction]): Unit = ()
}
