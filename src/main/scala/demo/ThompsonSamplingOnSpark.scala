package epsilon.demos

import org.apache.spark.sql.SparkSession

object ThompsonSamlingOnSpakr{
  val spark: SparkSession = SparkSession.builder.master("local").appName("TemperatureViewer").getOrCreate()
  import spark.implicits._
  

}

