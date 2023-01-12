package ada.interface

import ada._


trait Model[ModelData, ModelAction]{
    def report: String = this.toString
}

trait SimpleModel[ModelData, ModelAction] extends Model[ModelData, ModelAction]{
    def update(data: ModelData, action:  Tree[ModelAction]): Unit
    def act(data: ModelData): Tree[ModelAction]
}

trait ContextualModel[ModelID, Context, ModelData, ModelAction] 
    extends Model[ModelData, ModelAction]{
    def update(modelIds: Tree[ModelID], context: Context, data: ModelData, reward: Reward): Unit
    def update(modelIds: Tree[ModelID], context: Context, data: ModelData, action:  Tree[ModelAction]): Unit
    def actWithID(context: Context, data: ModelData, modelIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID])
}

trait StackableModel[ModelID, ModelData, ModelAction]
    extends Model[ModelData, ModelAction]{
    def update(modelIds: Tree[ModelID], data: ModelData, action:  Tree[ModelAction]): Unit
    def update(modelIds: Tree[ModelID], data: ModelData, reward: Reward): Unit
    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): (Tree[ModelAction], Tree[ModelID])
}


trait ContextualModelPassive[ModelID, Context, ModelData, ModelAction, AggregateReward]
    extends ContextualModel[ModelID, Context, ModelData, ModelAction]{
        def update(context: Context, data: ModelData, optimalAction:  Tree[ModelAction]): Unit
        def evaluate(action: Tree[ModelAction], optimalAction:  Tree[ModelAction]): ada.Reward
        def updateAll(modelIds: Tree[ModelID], context: Context,  data: ModelData, optimalAction:  Tree[ModelAction]): Unit
}



trait StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModel[ModelID, ModelData, ModelAction]{
        def update(data: ModelData, optimalAction:  Tree[ModelAction]): Unit
        def evaluate(action:  Tree[ModelAction],optimalAction:  Tree[ModelAction]): ada.Reward
        def updateAll(modelIds: Tree[ModelID], data: ModelData, optimalAction:  Tree[ModelAction]): Unit
}


trait StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    extends StackableModelPassive[ModelID, ModelData, ModelAction, AggregateReward]{
    def update(modelIds: Tree[ModelID], data: ModelData, action:  Tree[ModelAction]): Unit
    def updateAll(modelIds: Tree[ModelID], data: ModelData, optimalAction:  Tree[ModelAction]): Unit = {
            update(data, optimalAction)
        }
}



trait InertModel[ModelID, ModelData, ModelAction]{
    def update(modelIds: Tree[ModelID], data: ModelData, action:  Tree[ModelAction]): Unit =  ()
    def update(modelIds: Tree[ModelID], data: ModelData, reward: Reward): Unit =  ()
    def update(modelIds: Tree[ModelID],reward: Reward): Unit =  ()
    def update(data: ModelData, reward: Reward): Unit =  ()
    def update(reward: Reward): Unit =  ()
    def update(data: ModelData, action:  Tree[ModelAction]): Unit = ()
    def evaluate(action:  Tree[ModelAction], optimalAction: Tree[ModelAction]): ada.Reward = new Reward(0.0)
}

trait InertModelContext[ModelID, Context, ModelData, ModelAction] extends InertModel[ModelID, ModelData, ModelAction]{
    def update(modelIds: Tree[ModelID], context: Context, data: ModelData, reward: Reward): Unit = ()
    def update(modelIds: Tree[ModelID], context: Context, data: ModelData, action:  Tree[ModelAction]): Unit = ()
}
