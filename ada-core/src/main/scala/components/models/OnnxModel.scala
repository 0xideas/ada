package ada.core.models

import ada._

import io.circe.Json

import ada.core.interface._

import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import ai.onnxruntime.OnnxTensor

import scala.jdk.CollectionConverters._
import ai.onnxruntime.OrtUtil

trait InertModel[ModelID, ModelData, ModelAction]{
    def update(modelIds: List[ModelID], data: ModelData, reward: Reward): Unit =  ()
    def update(modelIds: List[ModelID],reward: Reward): Unit =  ()
    def update(data: ModelData, reward: Reward): Unit =  ()
    def update(reward: Reward): Unit =  ()
}


class OnnxRegression[ModelID, ModelData, ModelAction, AggregateReward](path: String, input_name: String)
    extends StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    with SimpleModel[ModelData, ModelAction]
    with InertModel[ModelID, ModelData, ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: ModelData): ModelAction = {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map(input_name -> inputs).asJava)
        //predictions
        result.get(0).getValue().asInstanceOf[Array[Array[ModelAction]]](0)(0)
    }

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = 
        (act(data), selectedIds)

    def export: Json = Json.fromString(loadingTime + "   " + path)

}

class OnnxClassifier[ModelID, ModelData, ModelAction, AggregateReward]
    (path: String,
    input_name: String,
    modelActionFn: Int => ModelAction)
    extends StackableModelPassiveBottom[ModelID, ModelData, ModelAction, AggregateReward]
    with SimpleModel[ModelData, ModelAction]
    with InertModel[ModelID, ModelData, ModelAction]{

    val env = OrtEnvironment.getEnvironment();
    val session = env.createSession(path ,new OrtSession.SessionOptions());
    val loadingTime = java.time.LocalDateTime.now().toString()

    def act(data: ModelData): ModelAction= {
        val inputs = OnnxTensor.createTensor(env, data)
        val result = session.run(Map(input_name -> inputs).asJava)
        //predictions
        val probabilities = result.get(0).getValue().asInstanceOf[Array[Array[Float]]](0)
        modelActionFn(probabilities.indexOf(probabilities.max))
    }

    def actWithID(data: ModelData, selectedIds: List[ModelID]): (ModelAction, List[ModelID]) = 
        (act(data), selectedIds)

    def export: Json = Json.fromString(loadingTime + "   " + path)

}
