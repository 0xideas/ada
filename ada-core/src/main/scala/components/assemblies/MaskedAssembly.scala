package ada.assemblies

import breeze.stats.mode
import io.circe.Json
import io.circe.syntax._

import ada._
import ada.interface._
import ada.components.selectors._
import ada.components.distributions._
import scala.collection.mutable.ListBuffer

import io.circe.Decoder

class MaskedStackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward <: Updateable](
    ensembles: List[StackableModel[ModelID, ModelData, ModelAction] with MaskedSelector[ModelID, ModelData, ModelAction]])
    extends StackableAssembly1[ModelID, ModelData, ModelAction, AggregateReward](ensembles){
    private def updateMask(ids: Tree[ModelID], mask: ListBuffer[ModelID]): Unit = {
    ids match {
        case Leaf(value) => mask.append(value)
        case Twig(value, _) => mask.append(value)
        case Branch(values) => values.map{value => updateMask(value, mask)}
    }
    } 
    override def actWithID(data: ModelData, selectedIds: Tree[ModelID]):  (Tree[ModelAction], Tree[ModelID]) = {
        val maskK : ListBuffer[ModelID] = ListBuffer()
        val actionTuple = ensembles.zipWithIndex.map{case(ensemble, depth) => {
            ensemble.setMask(maskK)
            val (action, ids) = ensemble.actWithID(data, selectedIds)
            updateMask(ids, maskK)
            (action, ids)
        }}
        val (actions, idTuples) = actionTuple.toList.unzip
        (new Branch(actions), new Branch(idTuples))
    }

}
