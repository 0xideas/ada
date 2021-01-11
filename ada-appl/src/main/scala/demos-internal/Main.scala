package ada.demos

import scala.collection.mutable.{ListBuffer}
//import ada.demos.{StackedBayesianRegressionContextDemo => Demo}
import ada.demos.{OnlineModelSelection => Demo}
object Main extends App{
    var i = 0
    var demo = new Demo()
    while(i < 2){
        demo.run()
        demo = new Demo()
        i += 1
    }

}
