package epsilon.core.components.learners

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import epsilon._
import epsilon.core.interface._
import epsilon.core.components.distributions._



trait GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward] 
    extends AbstractGreedy[ModelID, ModelData, ModelAction, AggregateReward]
    with ExploreWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]


