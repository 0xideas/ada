
package object epsilon {
    type Reward = Double
    type AggregateReward = Double
    type Probability = Double

    def printEpsilon(s: String): Unit =
        println(s)

}

/*
    def modelRewardsFromHistory[ModelData, ModelAction](rewardHistory: Model[ModelData, ModelAction] => Iterable[Reward],
                                aggregateRewards: Iterable[Reward] => AggregateReward): Model[ModelData, ModelAction] => AggregateReward = {
        (aggregateRewards compose rewardHistory)
    }
*/