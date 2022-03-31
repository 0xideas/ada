package ada.assemblies

import breeze.stats.mode
import io.circe.Json
import io.circe.syntax._

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._


abstract class Assembly[ModelID, ModelData, ModelAction, AggregateReward](
    val ensembles: List[StackableModel[ModelID, ModelData, ModelAction]])
    extends StackableModel[ModelID, ModelData, ModelAction]{
    def ensembles(modelId: ModelID): StackableModel[ModelID, ModelData, ModelAction] = {
        throw new Exception("ensembles in Assembly do not have an ID. Try a depth Integer instead")
    }
    def getEnsembles: List[StackableModel[ModelID, ModelData, ModelAction]] = ensembles
    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): (Tree[ModelAction], Tree[ModelID]) = {
        val actionTuple = ensembles.toList.map{ensemble => {
            val (action, modelIds_) = ensemble.actWithID(data, selectedIds)
            (action, modelIds_)
        }}
        val (actions, idTuples) = actionTuple.toList.unzip
        (new Branch(actions), new Branch(idTuples))
    }
    def update(modelIds: Tree[ModelID], data: ModelData, reward: Reward): Unit = {
        modelIds match {
            case Branch(branches) => {
                branches.zipWithIndex.map{case(branch, depth) =>
                    ensembles(depth).update(branch, data, reward)
                }
            }
            case _ => throw new Exception("Assembly update modelIds must be Branch")
        }  
    }
    def update(modelIds: Tree[ModelID], data: ModelData, action: Tree[ModelAction]): Unit = {
        modelIds match {
            case Branch(branches) => {
                branches.zipWithIndex.map{case(branch, depth) =>
                    ensembles(depth).update(branch, data, action)
                }
            }
            case _ => throw new Exception("Assembly update modelIds must be Branch")
        }
    }
}


class StackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward <: Updateable](
    ensembles: List[StackableModel[ModelID, ModelData, ModelAction]])
    extends Assembly[ModelID, ModelData, ModelAction, AggregateReward](ensembles)

class StackableAssembly2[ModelID, ModelData, ModelAction, AggregateReward <: UpdateableContext[ModelData]](
    ensembles: List[StackableModel[ModelID, ModelData, ModelAction]])
    extends Assembly[ModelID, ModelData, ModelAction, AggregateReward](ensembles)
