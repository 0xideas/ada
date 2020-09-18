package epsilon

import scala.collection.mutable.{Map => MutableMap}
trait Model {
    def act(data: ModelData): ModelAction
}

abstract class EpsilonEnsembleInterface (epsilon: Double, models: Iterable[Model]) {
    def act(data: ModelData): ModelAction

    def modelRewardsFromHistory(rewardHistory: Model => Iterable[Reward],
                                aggregateRewards: Iterable[Reward] => AggregateReward): Model => AggregateReward = {
        (aggregateRewards compose rewardHistory)
    }
}