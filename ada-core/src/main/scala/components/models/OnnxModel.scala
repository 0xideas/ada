package ada.core.models

import ada._

import io.circe.Json

import ada.core.interface._

import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import ai.onnxruntime.OnnxTensor

import scala.jdk.CollectionConverters._
import ai.onnxruntime.OrtUtil


class OnnxRegression[ModelID, ModelDataElement, ModelAction](path: String, input_name: String)
    extends StackableModel[ModelID, Array[Array[ModelDataElement]], ModelAction]
    with StackableModel2[ModelID, Array[Array[ModelDataElement]], ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: Array[Array[ModelDataElement]]): ModelAction = {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map(input_name -> inputs).asJava)
        //predictions
        result.get(0).getValue().asInstanceOf[Array[Array[ModelAction]]](0)(0)
    }

    def update(modelIds: List[ModelID], data: Array[Array[ModelDataElement]], reward: Reward): Unit =  ()
    def update(modelIds: List[ModelID], reward: Reward): Unit =  ()

    def actWithID(data: Array[Array[ModelDataElement]], selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = 
        (act(data), selectedIds)

    def export: Json = Json.fromString(loadingTime + "   " + path)

}

class OnnxClassifier[ModelID, ModelDataElement, ModelAction]
    (path: String,
    input_name: String,
    modelActionFn: Int => ModelAction)
    extends StackableModel[ModelID, Array[Array[ModelDataElement]], ModelAction]
    with StackableModel2[ModelID, Array[Array[ModelDataElement]], ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: Array[Array[ModelDataElement]]): ModelAction= {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map(input_name -> inputs).asJava)
        //predictions
        val probabilities = result.get(0).getValue().asInstanceOf[Array[Array[Float]]](0)
        modelActionFn(probabilities.indexOf(probabilities.max))
    }

    def update(modelIds: List[ModelID], data: Array[Array[ModelDataElement]], reward: Reward): Unit =  ()
    def update(modelIds: List[ModelID], reward: Reward): Unit =  ()

    def actWithID(data: Array[Array[ModelDataElement]], selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = 
        (act(data), selectedIds)

    def export: Json = Json.fromString(loadingTime + "   " + path)

}
