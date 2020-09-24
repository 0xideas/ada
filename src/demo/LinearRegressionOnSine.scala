package epsilon

import models.LinearRegression.SimpleLinearRegressionModel
import main.scala.epsilonEnsemble.EpsilonEnsembleLocal
import dataGenerators.TimeseriesGenerators.SineGenerator

object Main{
    val models = List(SimpleLinearRegression(-3.0),
                    SimpleLinearRegression(-2.0),
                    SimpleLinearRegression(-1.0),
                    SimpleLinearRegression(0.0),
                    SimpleLinearRegression(1.0),
                    SimpleLinearRegression(2.0),
                    SimpleLinearRegression(3.0))
    
    val ensemble = EpsilonEnsembleLocal[Double, Double](epsilon=0.2, models, (aggRew, rew)=>rew)

    val generator = SineGenerator(1.0, 1000)

    private var action = Double
    while(true){
        action = ensemble.act(generator.next())
        ensemble.update()
    }
    


}
