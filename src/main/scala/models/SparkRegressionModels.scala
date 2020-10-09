package epsilon.models

import org.apache.spark.ml.{Estimator, Model}
import org.apache.spark.ml.regression.LinearRegression
import org.apache.spark.mllib.regression.LinearRegressionModel
import org.apache.spark.mllib.linalg.Vector


import org.apache.spark.sql.SparkSession
import epsilon.interfaces.{Model => ModelEE}

//import com.fasterxml.jackson.annotation.JsonCreator.Mode

class LinearRegressionModelEE(val model:LinearRegressionModel) extends ModelEE[Vector, Double] {
    def act(data: Vector): Double = model.predict(data)
}

object LinearRegressionModelEE{
    def apply(weights:Vector, intercept:Double): LinearRegressionModelEE = {
        val model = new LinearRegressionModel(weights, intercept)
        new LinearRegressionModelEE(model)
    }
    def apply(model: LinearRegressionModel): LinearRegressionModelEE = {
        new LinearRegressionModelEE(model)
    }
}

trait SparkModelMaker[M <: Model[M], ModelEE2] extends Estimator[M]{
  override def fit(dataset: org.apache.spark.sql.Dataset[_])(implicit toModel: M => ModelEE2): ModelEE2 = {
      val model = super.fit(dataset)
      model
  }
  override def fit(dataset: org.apache.spark.sql.Dataset[_],paramMaps: Array[org.apache.spark.ml.param.ParamMap])(implicit toModel: M => ModelEE2): Seq[ModelEE2] = {
      val model = super.fit(dataset, paramMaps)
      model.map(toModel)
  }
  override def fit(dataset: org.apache.spark.sql.Dataset[_],paramMap: org.apache.spark.ml.param.ParamMap)(implicit toModel: M => ModelEE2): ModelEE2 = {
      val model = super.fit(dataset, paramMap)
      model
  }
  override def fit(dataset: org.apache.spark.sql.Dataset[_],firstParamPair: org.apache.spark.ml.param.ParamPair[_],otherParamPairs: org.apache.spark.ml.param.ParamPair[_]*)(implicit toModel: M => ModelEE2): ModelEE2 = {
      val model = super.fit(dataset, firstParamPair, otherParamPairs:_*)
      model
  }

}

class LinearRegressionEE extends LinearRegression with SparkModelMaker[LinearRegressionModel, LinearRegressionModelEE]

object Main{
    implicit def toModel(m: LinearRegressionModel): LinearRegressionModelEE = LinearRegressionModelEE(m)

    val spark: SparkSession = SparkSession.builder.master("local").appName("test").getOrCreate()
    import spark.implicits._

    val lr = new LinearRegressionEE()


    lr.setMaxIter(10)
        .setRegParam(0.3)
        .setElasticNetParam(0.8)

    val training = spark.read.format("libsvm")
      .load("data/mllib/sample_linear_regression_data.txt")

    val model = lr.fit(training)
    val predictions = model.model.transform(training)
    val actions = model.act(training)

}