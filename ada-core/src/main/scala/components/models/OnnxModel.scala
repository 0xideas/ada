package components.models

import ada._

import io.circe.Json

import ada.core.interface._

import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import ai.onnxruntime.OnnxTensor

import scala.jdk.CollectionConverters._
import ai.onnxruntime.OrtUtil


class GenericOnnxModel[ModelID, ModelDataElement, ModelAction](path: String)
    extends StackableModel[ModelID, Array[Array[ModelDataElement]], ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    //var session = env.createSession("/home/leon/data/onnx/dt_iris.onnx",new OrtSession.SessionOptions());
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: Array[Array[ModelDataElement]]): ModelAction = {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map("float_input" -> inputs).asJava)

        //predictions
        result.get(0).getValue().asInstanceOf[Array[Array[ModelAction]]](0)(0)
    }

    def update(modelIds: List[ModelID], data: Array[Array[ModelDataElement]], reward: Reward): Unit =  ()

    def actWithID(data: Array[Array[ModelDataElement]], selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = 
        (act(data), selectedIds)

    def export: Json = Json.fromString(loadingTime + "   " + path)

}
