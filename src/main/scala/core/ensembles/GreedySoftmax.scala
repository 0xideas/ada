package epsilon.ensembles

import scala.collection.mutable.{Map => MutableMap}
import epsilon.interfaces.{EpsilonEnsemblePassive, Model, LocalEnsemble}

import epsilon._
import breeze.stats.mode
import epsilon.distributions.ContextualDistribution


trait SelectModel[ModelID, ModelData, ModelAction, AggregateReward]{
    protected val rnd = new scala.util.Random(101)

    def _sortModel(models: Map[ModelID, Model[ModelData, ModelAction]],
                 modelRewards: ModelID => AggregateReward,
                 draw: AggregateReward => Double): List[(ModelID, Double)] = {
        val modelIds = models.keys
        val modelsSorted = modelIds.map(modelId => (modelId, draw(modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)
        modelsSorted
    }

    def _sortModel[Context](models: Map[ModelID, Model[ModelData, ModelAction]],
                            modelRewards: ModelID => AggregateReward,
                            context: Context,
                            draw: (Context, AggregateReward) => Double): List[(ModelID, Double)] = {
        val modelIds = models.keys
        val modelsSorted = modelIds.map(modelId => (modelId, draw(context, modelRewards(modelId))))
                                        .toList
                                        .sortWith(_._2 > _._2)
        modelsSorted
    }

    def _selectModel(models: Map[ModelID, Model[ModelData, ModelAction]],
            aggregateRewardsDouble: List[(ModelID, Double)],
            data: ModelData): (ModelAction, ModelID)
}

trait ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    extends SelectModel[ModelID, ModelData, ModelAction, AggregateReward]{

    def _selectModel(models: Map[ModelID, Model[ModelData, ModelAction]],
                            aggregateRewardsDouble: List[(ModelID, Double)],
                            data: ModelData): (ModelAction, ModelID) = {
        val totalReward: Double = aggregateRewardsDouble.foldLeft(0.0)((agg, tup) => agg + tup._2)
        val cumulativeProb: List[(Probability, Probability)] = aggregateRewardsDouble.scanLeft((0.0, 0.0))((acc, item) =>  (acc._2, acc._2 + item._2/totalReward)).tail
        val modelsCumulativeProb: List[(ModelID, (Probability, Probability))] = aggregateRewardsDouble.map(_._1).zip(cumulativeProb)
        val selector = rnd.nextDouble()
        val selectedModelId: ModelID = modelsCumulativeProb.filter{case(model, bounds) => (selector >= bounds._1) && (selector <= bounds._2)}(0)._1
        val selectedModel: Model[ModelData, ModelAction] = models(selectedModelId)
        (selectedModel.act(data), selectedModelId)
    }
}

//not used so far
trait Softmax[ModelID, ModelData, ModelAction, AggregateReward]
    extends ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]{
    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]],
                 modelRewards: ModelID => AggregateReward,
                 draw: AggregateReward => Double,
                 data: ModelData): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel(models, modelRewards, draw)
        _selectModel(models, modelsSorted, data)
    }
    def _actImpl[Context](models: Map[ModelID, Model[ModelData, ModelAction]],
                 modelRewards: ModelID => AggregateReward,
                 context: Context,
                 draw: (Context, AggregateReward) => Double,
                 data: ModelData): (ModelAction, ModelID) = {
        val modelsSorted = _sortModel[Context](models, modelRewards, context, draw)
        _selectModel(models, modelsSorted, data)
    }
}


trait AbstractGreedy[ModelID, ModelData, ModelAction, AggregateReward] extends SelectModel[ModelID, ModelData, ModelAction, AggregateReward] {
    def _actImpl(models: Map[ModelID, Model[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                draw: AggregateReward => Double,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel(models, modelRewards, draw)
        
        if(rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelsSorted.tail, data)
    }
    def _actImpl[Context](models: Map[ModelID, Model[ModelData, ModelAction]],
                modelRewards: ModelID => AggregateReward,
                context: Context,
                draw: (Context, AggregateReward) => Double,
                epsilon: Double,
                data: ModelData): (ModelAction, ModelID) = {

        val modelsSorted = _sortModel[Context](models, modelRewards, context, draw)
        
        if(rnd.nextDouble() > epsilon) {
            val selectedModelId = modelsSorted.head._1
            val selectedModel = models(selectedModelId)
            (selectedModel.act(data), selectedModelId)
        }
        else _selectModel(models, modelsSorted.tail, data)
    }
}

trait GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward] 
    extends AbstractGreedy[ModelID, ModelData, ModelAction, AggregateReward]
    with ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]


class EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: AggregateReward => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward)
    extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, AggregateReward]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction] {

    def actWithID(data: ModelData): (ModelAction, ModelID) = _actImpl(models, modelRewards, draw, epsilon, data)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)

    def updateAll(data: ModelData,
              correct: ModelAction): Unit = _updateAllImpl(data, correct, models, modelRewards)
    
    def getModelRewards: MutableMap[ModelID, AggregateReward] = modelRewards

    def update(modelId: ModelID, reward: Reward): Unit = updateFn(modelId, reward, modelRewards,  updateAggregateRewardFn)

}

object EpsilonEnsembleGreedySoftmaxLocal{
    def apply[ModelID, ModelData, ModelAction, AggregateReward](
        models: Map[ModelID, Model[ModelData, ModelAction]],
        initAggregateRewards: AggregateReward,
        draw: AggregateReward => Double,
        epsilon: Double,
        evaluationFn: (ModelAction, ModelAction) => Reward,
        updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward){
            val modelRewards = MutableMap(models.toSeq.map{case(k,v) => (k, initAggregateRewards)}:_*)
            new EpsilonEnsembleGreedySoftmaxLocal[ModelID, ModelData, ModelAction, AggregateReward](models, modelRewards, draw, epsilon, evaluationFn, updateAggregateRewardFn)
        }
}


class EpsilonEnsembleGreedySoftmaxLocalWithContext[ModelID, Context, ModelData, ModelAction, AggregateReward <: ContextualDistribution[Context, Reward]]
    (models: Map[ModelID, Model[ModelData, ModelAction]],
    modelRewards: MutableMap[ModelID, AggregateReward],
    draw: (Context, AggregateReward) => Double,
    epsilon: Double,
    evaluationFn: (ModelAction, ModelAction) => Reward,
    updateAggregateRewardFn: (AggregateReward, Reward) => AggregateReward)
    extends EpsilonEnsemblePassive[ModelID, ModelData, ModelAction, AggregateReward]
    with GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward]
    with LocalEnsemble[ModelID, ModelData, ModelAction] {

    def actWithID(context: Context, data: ModelData): (ModelAction, ModelID) = _actImpl[Context](models, modelRewards, context, draw, epsilon, data)

    def evaluate(action: ModelAction, optimalAction: ModelAction): Reward = evaluationFn(action, optimalAction)

    def updateAll(context: Context, data: ModelData,
                correct: ModelAction): Unit = _updateAllImpl[Context](context, data, correct, models, modelRewards)
    
    def getModelRewards: MutableMap[ModelID, AggregateReward] = modelRewards

    def update(modelId: ModelID, context: Context, reward: Reward): Unit = updateFn[Context, AggregateReward](modelId, context, reward: Reward, modelRewards: ModelID => AggregateReward)

}