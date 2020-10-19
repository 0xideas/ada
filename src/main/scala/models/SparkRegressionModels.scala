package epsilon.models

import org.apache.spark.ml.{Estimator, Model}
import org.apache.spark.ml.regression.{LinearRegression, LinearRegressionModel}
import org.apache.spark.ml.linalg.{Vector, Vectors}
import org.apache.spark.rdd.RDD

import org.apache.spark.sql.SparkSession
import epsilon.interfaces.{Model => ModelEE}
import org.apache.spark.ml.param.ParamMap
import org.apache.spark.sql.Dataset
import org.apache.spark.ml.param.ParamPair
import shapeless.Data


//inheritance does not work because it is impossible to alter the factory to output that new class
//it would still be nice to automatically refer to model.method
class LinearRegressionModelEE(val model:LinearRegressionModel, factory: LinearRegressionEE) extends ModelEE[Vector, Double] {

    def act(data: Vector): Double = model.predict(data)
    def fit(data: Dataset[_]): LinearRegressionModelEE ={       
        factory.fitEE(data,  model.extractParamMap() )
    } 
}

class LinearRegressionEE extends LinearRegression{
    implicit def toModel(m:LinearRegressionModel) = new LinearRegressionModelEE(m, this)
    def fitEE(dataset: Dataset[_], paramMap: ParamMap): LinearRegressionModelEE = new LinearRegressionModelEE(super.fit(dataset, paramMap), this)
    def fitEE(dataset: Dataset[_]): LinearRegressionModelEE = new LinearRegressionModelEE(super.fit(dataset), this)

    /*override def fit(dataset: Dataset[_], paramMaps: Array[ParamMap]): Seq[LinearRegressionModelEE] = super.fit(dataset, paramMaps).map(new LinearRegressionModelEE(_, this))
    override def fit(dataset: Dataset[_], firstParamPair: ParamPair[_], otherParamPairs: ParamPair[_]*): LinearRegressionModelEE = new LinearRegressionModelEE(super.fit(dataset, firstParamPair, otherParamPairs:_*), this)
    override def fit(dataset: Dataset[_], paramMap: ParamMap): LinearRegressionModelEE = new LinearRegressionModelEE(super.fit(dataset, paramMap), this)
    override def fit(dataset: Dataset[_]): LinearRegressionModelEE =  new LinearRegressionModelEE(super.fit(dataset), this)*/
}
