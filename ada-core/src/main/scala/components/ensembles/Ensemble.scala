package ada.interface

import ada.Reward
import ada.components.distributions.ConditionalDistribution


abstract class AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward])
    extends Model[ModelData, ModelAction]{
    def getModels(): Map[ModelID, Model[ModelData, ModelAction]] = models
    def modelRewards(): Map[ModelID, AggregateReward] = modelRewards
    def modelRewards(id: ModelID):  AggregateReward = modelRewards()(id)
}


abstract class SimpleEnsemble[ModelID, ModelData, ModelAction, AggregateReward <: Updateable]
    (models: Map[ModelID, SimpleModel[ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelRewards){
    def actWithID(data: ModelData): ( Tree[ModelAction], ModelID)
    def act(data: ModelData):  Tree[ModelAction] = actWithID(data)._1
    def update(modelId: ModelID, reward: Reward): Unit = modelRewards(modelId).update(reward)
    def update(modelId: ModelID, data: ModelData, action:  Tree[ModelAction]): Unit = {
        models(modelId).update(data, action)
    }
}


abstract class ContextualEnsemble[ModelID, Context, ModelData, ModelAction, AggregateReward <: UpdateableContext[Context]]
    (models: Map[ModelID, ContextualModel[ModelID, Context, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID,  ModelData, ModelAction, AggregateReward](models, modelRewards)
    with ContextualModel[ModelID, Context, ModelData, ModelAction]{
    def actWithID(context: Context, data: ModelData, modelIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID])
    def act(modelIds: Tree[ModelID], context: Context, data: ModelData):  Tree[ModelAction] = actWithID(context, data, modelIds)._1
    def update(modelIds: Tree[ModelID], context: Context, data: ModelData, reward: Reward): Unit = {
        modelIds match {
            case Twig(value, branch) => {
                modelRewards(value).update(context, reward)
                models(value).update(branch, context, data, reward)
            }
            case Leaf(value) => {
                modelRewards(value).update(context, reward)
            }
            case _ => throw new Exception("Ensemble update modelIds must be Leaf or Twig")
        }
    }
    def update(modelIds: Tree[ModelID], context: Context, data: ModelData, action: Tree[ModelAction]): Unit = {
        modelIds match {
            case Twig(value, branch) => {
                val m = models(value)
                models(value).update(branch, context, data, action)
            }

        }
    }
}


abstract class StackableEnsemble1[ModelID, ModelData, ModelAction, AggregateReward <: Updateable](
    models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with StackableModel[ModelID, ModelData, ModelAction]{
    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID])
    def act(data: ModelData, selectedIds: Tree[ModelID]):  Tree[ModelAction] = actWithID(data, selectedIds)._1
    def act(data: ModelData)(implicit dummyId: ModelID):  Tree[ModelAction] = actWithID(data, new Leaf(dummyId))._1
    def update(modelIds: Tree[ModelID], data: ModelData, reward: Reward): Unit = {        
        modelIds match {
            case Twig(value, branch) => {
                modelRewards(value).update(reward)
                models(value).update(branch, data, reward)
            }
            case Leaf(value) => {
                modelRewards(value).update(reward)
            }
            case _ => throw new Exception("Ensemble update modelIds must be Leaf or Twig")
        }   
    }
    def update(modelIds: Tree[ModelID], data: ModelData, action:  Tree[ModelAction]): Unit = {
        modelIds match {
            case Twig(value, branch) => {
                models(value).update(branch, data, action)
            }
            case Leaf(value) => ()
            case _ => throw new Exception("Ensemble update modelIds must be Leaf or Twig")
        }
    }
}


abstract class StackableEnsemble2[ModelID, ModelData, ModelAction, AggregateReward <: UpdateableContext[ModelData]](
    models: Map[ModelID, StackableModel[ModelID, ModelData, ModelAction]],
    modelRewards: Map[ModelID, AggregateReward])
    extends AdaEnsemble[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards)
    with StackableModel[ModelID, ModelData, ModelAction]{
    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): ( Tree[ModelAction], Tree[ModelID])
    def act(data: ModelData, selectedIds: Tree[ModelID]): Tree[ModelAction] = actWithID(data, selectedIds)._1
    def act(data: ModelData)(implicit dummyId: ModelID): Tree[ModelAction] = actWithID(data, new Leaf(dummyId))._1
    def update(modelIds: Tree[ModelID], data: ModelData, reward: Reward): Unit = {
        modelIds match {
            case Twig(value, branch) => {
                modelRewards(value).update(data, reward)
                models(value).update(branch, data, reward)
            }
            case Leaf(value) => {
                modelRewards(value).update(data, reward)
            }
            case _ => throw new Exception("Ensemble update modelIds must be Leaf or Twig")
        }
    }
    def update(modelIds: Tree[ModelID], data: ModelData, action: Tree[ModelAction]): Unit = {
        modelIds match {
            case Twig(value, branch) => {
                models(value).update(branch, data, action)
            }
            case Leaf(value) => ()
        }
    }

}


