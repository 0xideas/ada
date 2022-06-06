package ada.models

import ada._

import io.circe.Json

import ada.interface._

import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import ai.onnxruntime.OnnxTensor

import scala.jdk.CollectionConverters._
import ai.onnxruntime.OrtUtil



class OnnxRegression[ModelID, ModelData, ModelAction, AggregateReward](
    val path: String,
    input_name: String,
    modelActionFn: Double => ModelAction)
    extends StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    with SimpleModel[ModelData, ModelAction]
    with InertModel[ModelID, ModelData, ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: ModelData): Tree[ModelAction] = {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map(input_name -> inputs).asJava)
        //predictions
        new Leaf(result.get(0).getValue().asInstanceOf[Array[Array[ModelAction]]](0)(0))
    }
    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): (Tree[ModelAction], Tree[ModelID]) = 
        (act(data), selectedIds)
    def export: Json = Json.fromString(loadingTime + "   " + path)
}

class OnnxClassifier[ModelID, ModelData, ModelAction, AggregateReward]
    (val path: String,
    input_name: String,
    modelActionFn: Int => ModelAction)
    extends StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    with SimpleModel[ModelData, ModelAction]
    with InertModel[ModelID, ModelData, ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: ModelData): Tree[ModelAction] = {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map(input_name -> inputs).asJava)
        //predictions
        val probabilities = result.get(0).getValue().asInstanceOf[Array[Array[Float]]](0)
        new Leaf(modelActionFn(probabilities.indexOf(probabilities.max)))
    }

    def actWithID(data: ModelData, selectedIds: Tree[ModelID]): (Tree[ModelAction], Tree[ModelID]) = 
        (act(data), selectedIds)

    def export: Json = Json.fromString(loadingTime + "   " + path)
}
