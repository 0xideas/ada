package ada.demos

import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.ensembles.ThompsonSamplingEnsemble
import ada.models._
import ada.components.distributions.{Distribution, BetaDistribution}
import ada.interface._
import plotting.Chart
import ada.`package`._
import ada.assemblies.StackableAssembly1
import io.circe.Json
import io.circe.{Decoder, Encoder}

import ada.assemblies.MaskedStackableAssembly1
import ada.components.selectors.MaskedSelector
import cats.instances.option

import ada.products.PageGenerator


object HierarchicalNewssite{

    val nFields = 3
    val nArticles = 5
    val bounceParameter = 10 //the attractiveness of a different site relative to the newssite - people bouncing off the page

    val headlineCandidates = List(List("A New Planet Found", "A New World", "Our New Neighbour"),
                                  List("M.I.A is back", "The Excentric Rapper Making a Comeback", "M.I.A. out With New Banger"),
                                  List("The Pandemic is Over", "Freedom at last!", "Finally, the Pandemic Ends"),
                                  List("Beethovens 200th Birthday", "Celebrate this Classical Master", "The Bonn Miracle"),
                                  List("Stonks Up", "Stonks Rising", "Stonks!!!")
                             ).zipWithIndex.map{case(k, v) => (v,k)}.toMap

    val headlineProbabilities = List(List(9.3, 6.4, 4.8), 
                                    List(5.5, 5.1, 5.8),
                                    List(13.2, 15.7, 11.9),
                                    List(5.1, 4.9, 5.7),
                                    List(4.5, 3.6, 5.9))

    //headlineCandidates.zip(headlineProbabilities).map(t => t._1.zip(t._2)).map(_.toMap).flatten
    // List((Freedom at last!,1.0), (Finally, the Pandemic Ends,0.9), (The Pandemic is Over,0.9), (A New World,0.9), (Stonks Up,0.9))
    val rnd = scala.util.Random
    type ArticleID = Int
    type HeadlineID = Int

    val pageGenerator = new PageGenerator[String](nFields, headlineCandidates)(s => Json.fromString(s), Decoder[Int], Encoder[String], Decoder[String])
    def pickArticle(headlines: List[(ArticleID, String, Tree[HeadlineID])]): Option[ArticleID] = {
        val probabilities0 = headlines.map{
            case(articleId, headline, headlineIds) => headlineIds match {
                case( Leaf(headlineId)) => headlineProbabilities(articleId)(headlineId)
            } 
        }
        val probabilities = List(probabilities0(0)*2) ++ probabilities0.slice(1, probabilities0.length)
        val sumProbabilities = probabilities.sum + bounceParameter
        val probabilityThresholds = probabilities.map(_/sumProbabilities).scanLeft(0.0)((acc, v) => acc +v).tail
        val picker = rnd.nextDouble()
        val pickedArticle: Option[ArticleID] =  headlines.map(_._1).zip(probabilityThresholds).filter{case(articleId, p) => p >= picker} match {
            case head :: tail => Some(head._1)
            case _ => None
        }
        pickedArticle

    }

    def run(): Unit = {
        val shares = ListBuffer[List[Double]]()
        val buffer = ListBuffer[List[ArticleID]]()
        val nIter = 100 * 1000
        val interval = math.max(nIter/150, 50)
        (0 until nIter).map{i =>
            val page = pageGenerator.generate()
            val pickedArticle = pickArticle(page)
            pageGenerator.update(pickedArticle, page.map(p => (p._1, p._3)))
            buffer.append(page.map(_._1))

            if(i % interval == 0){
                val share = (0 until nArticles).map(articelId => buffer.map(b => if(b.contains(articelId)) 1.0 else 0.0).sum/(interval))
                shares.append(share.toList)
                buffer.clear()
            }
            println(page.map(_._2).mkString(" - "))
            println("\n")
            //println(page)
            //println(pickedArticle)
        }
        val symbols = "01234"
        val chart = Chart(1.1, -0.1, 0, right=nIter, width = 150, height = 40)
        (0 until nArticles).map{i => 
            chart.plotLine(shares.map(share => share(i)).toList, None, symbols(i).toString)
        }
        println(chart.render())

    }
}       
