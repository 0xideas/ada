package ada

import scala.collection.mutable.{ListBuffer}
import ada.demos.{SparseThompsonSampling => Demo}

object Main extends App{
    val runs = 25
    val results = (for{
        _ <- 0 until runs
    } yield {
        val res = Demo.run()
        res match {
            case(ensemble, nModels, nIter, shares) => Demo.report(ensemble, nModels, nIter, shares)
        }
        res
    }).toList

    type Share = ListBuffer[ListBuffer[Double]]
    val initB: Share = ListBuffer.fill(100)(ListBuffer.fill(100)(0.0))
    val reduced: Share = 
        results.map(_._4).foldLeft[Share](initB)( (agg: Share, shares: Share) => {
            agg.zipWithIndex.map{case (a,i) => (0 until a.length).map(j => a(j) += shares(i)(j)/runs )}; agg
        }) 
    
    //println("EXAMPLES")
    //results.take(5).map{case(ensemble, nModels, nIter, shares) => Demo.report(ensemble, nModels, nIter, shares)}
    println(f"EXPECTED VALUES OVER ${results.length} runs")
    Demo.report(results(0)._1, results(0)._2, results(0)._3, reduced)
}
