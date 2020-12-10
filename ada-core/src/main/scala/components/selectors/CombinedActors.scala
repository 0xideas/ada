package ada.core.components.selectors

import scala.collection.mutable.{Map => MutableMap}
import org.apache.commons.math3.stat.descriptive.AggregateSummaryStatistics

import ada._
import ada.core.interface._
import ada.core.components.distributions._

trait GreedySoftmax[ModelID, ModelData, ModelAction] 
    extends AbstractGreedy[ModelID, ModelData, ModelAction]
    with SoftmaxSelector[ModelID, ModelData, ModelAction]
