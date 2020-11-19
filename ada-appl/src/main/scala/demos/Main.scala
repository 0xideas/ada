package ada

import scala.collection.mutable.{ListBuffer}
import ada.demos.{SparseThompsonSampling => Demo}

object Main extends App{
    val runs = 50
    val results = (for{
        _ <- 0 until runs
    } yield Demo.run()).toList

    type Share = ListBuffer[ListBuffer[Double]]
    val initB: Share = ListBuffer.fill(10)(ListBuffer.fill(100)(0.0))
    val reduced: Share = 
        results.foldLeft[Share](initB)( (agg: Share, shares: Share) => {
            agg.zipWithIndex.map{case (a,i) => (0 until a.length).map(j => a(j) += shares(i)(j)/runs )}; agg
        }) 
    
    println("EXAMPLES")
    results.take(5).map(shares => Demo.report(shares))
    println(f"EXPECTED VALUES OVER ${results.length} runs")
    Demo.report(reduced)
}
