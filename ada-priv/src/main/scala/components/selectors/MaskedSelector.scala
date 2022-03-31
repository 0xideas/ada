package ada.components.selectors

import scala.collection.mutable.ListBuffer


trait MaskedSelector[ModelID, ModelData, ModelAction] 
    extends Selector[ModelID, ModelData, ModelAction]{
    def setMask(maskX: ListBuffer[ModelID]): Unit = {
        mask.clear()
        maskX.map(id => mask += id)
    }
}