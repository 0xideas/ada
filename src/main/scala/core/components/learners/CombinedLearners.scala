package ada.core.components.learners

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import ada._
import ada.core.interface._
import ada.core.components.distributions._

trait GreedySoftmax[ModelID, ModelData, ModelAction, AggregateReward] 
    extends AbstractGreedy[ModelID, ModelData, ModelAction, AggregateReward]
    with SelectWithSoftmax[ModelID, ModelData, ModelAction, AggregateReward]
