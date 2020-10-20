
package object epsilon {
    type Probability = Double
    type Reward = Double

    def printEpsilon(s: String): Unit =
        ()
        //println(s)

}

/*
    def modelRewardsFromHistory[ModelData, ModelAction](rewardHistory: Model[ModelData, ModelAction] => Iterable[Reward],
                                aggregateRewards: Iterable[Reward] => AggregateReward): Model[ModelData, ModelAction] => AggregateReward = {
        (aggregateRewards compose rewardHistory)
    }
*/