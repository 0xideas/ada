package ada.demos

import ada.demos.utility.Utilities
import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.PassiveThompsonSamplingEnsemble
import ada.core.ensembles.ThompsonSamplingEnsemble
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index
import ada.core.interface.StackableEnsemble1
import plotting.Chart

object CoordinationLearning{
    val nModelsLevel1 = 8
    val nModelsLevel2 = 3
    val nIter = 1000
    val learningRate = 0.05
    val strings = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")
    val payoffsBase = List(List(0.8, 0.7, 1.2, 1.25),
                            List(0.7, 0.6, 2.4, 0.4),
                            List(1.0, 1.1, 1.2, 1.0),
                            List(0.8, 1.7, 1.3, 1.1))

    /*val payoffsBase = List(List(1.1, 1.2, 1.3, 1.4),
                            List(0.9, 1.0, 1.1, 1.2),
                            List(0.7, 0.8, 0.9, 1.0),
                            List(0.5, 0.7, 0.8, 0.9))*/


    val payoff1step1 = payoffsBase.map(payoff => (payoff++ payoff.map(_+0.2)).slice(0, nModelsLevel2))
    val payoff1step2 = (payoff1step1 ++ payoff1step1.map(payoff => payoff.map(p =>p + 0.3))).slice(0, nModelsLevel1)
    val minPayoff = payoff1step2.flatMap(i => i).min
    val maxPayoff = payoff1step2.flatMap(i => i).map(i => i - minPayoff).max
    val payoffs1 = payoff1step2.map(payoffs => payoffs.map(payoff => (payoff-minPayoff)/maxPayoff))
    val payoffs2 = payoffs1.flatMap(i => i)

    require(strings.length >= math.max(nModelsLevel1, nModelsLevel2), "strings not sufficiently long")
    require(payoffs1.length == nModelsLevel1 && payoffs1.map(_.length == nModelsLevel2).reduce(_&&_), "payoffs incomplete")
    require(payoffs2.length == nModelsLevel1*nModelsLevel2)


    val models1 = (0 until nModelsLevel1*nModelsLevel2).map(i =>  new StaticModelString[Int, Unit, String](strings(i/nModelsLevel2)+strings(i%nModelsLevel2)))
    val ensembles1 = (0 until nModelsLevel1).map(i => new ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel2).zip(models1.slice(i*nModelsLevel2, (i+1)*nModelsLevel2)).toMap, 1, 1, learningRate))
    val ensemble1 = new ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel1).zip(ensembles1).toMap, 1, 1, 0.01)
    
    val models2 = (0 until nModelsLevel1*nModelsLevel2).map(i =>  new StaticModelString[Int, Unit, String](strings(i/nModelsLevel1)+strings(i%nModelsLevel1)))
    val ensemble2 = new ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel1*nModelsLevel2).zip(models2).toMap, 1, 1, learningRate)

    val models3_1 = (0 until nModelsLevel1).map(i =>  new StaticModelString[Int, Unit, String](strings(i)))
    val ensemble3_1 = new ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel1).zip(models3_1).toMap, 1, 1, learningRate)
    val models3_2 = (0 until nModelsLevel2).map(i =>  new StaticModelString[Int, Unit, String](strings(i)))
    val ensemble3_2 = new ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel2).zip(models3_2).toMap, 1, 1, learningRate)

    val selectionsList1 = ListBuffer[List[Double]](); val selectionsList2 = ListBuffer[List[Double]](); val selectionsList3 = ListBuffer[List[Double]]()
    val totalRewards1 = ListBuffer[Double](); val totalRewards2 = ListBuffer[Double](); val totalRewards3 = ListBuffer[Double]()


    def run(): Unit = {
        (0 until nIter).map{i => 
            val (action1, modelIds1) = ensemble1.actWithID((), List())
            val (action2, modelIds2) = ensemble2.actWithID((), List())

            val (action3_1, modelIds3_1) = ensemble3_1.actWithID((), List())
            val (action3_2, modelIds3_2) = ensemble3_2.actWithID((), List())
            val (action3, modelIds3) = (action3_1 + action3_2, modelIds3_1 ++ modelIds3_2)

            ensemble1.update(modelIds1, (), payoffs1(modelIds1(0))(modelIds1(1)))
            ensemble2.update(modelIds2, (), payoffs2(modelIds2(0)))

            val reward3 = payoffs1(modelIds3(0))(modelIds3(1))
            ensemble3_1.update(modelIds3_1, (), reward3 )
            ensemble3_2.update(modelIds3_2, (), reward3)
            //println( f"ensemble 3 : ${action3} reward ${reward3}")

            //println(f"${i}:   ${action1}   ${action2}")


            if(i % math.max((nIter/100), 1) == 0){
                val selections1 = Utilities.averageSelectedModelsLevel2(Utilities.selectStackable[Unit, String, BetaDistribution](ensemble1, (), 5, 1000), nModelsLevel1, nModelsLevel2)
                val selections2 = Utilities.selectAndAverageStackable[Unit, String, BetaDistribution](ensemble2, (), nModelsLevel1*nModelsLevel2, 1000)
                val selections3 = Utilities.averageSelectedModelsLevel2(Utilities.selectAndAverageStackableFromTwo[Unit, String, BetaDistribution](ensemble3_1, ensemble3_2, (), nModelsLevel1*nModelsLevel2, 1000), nModelsLevel1, nModelsLevel2)

                selectionsList1.append(selections1); selectionsList2.append(selections2); selectionsList3.append(selections3)
                totalRewards1.append(calculateTotalReward(selections1, payoffs2)); totalRewards2.append(calculateTotalReward(selections2, payoffs2)); totalRewards3.append(calculateTotalReward(selections3, payoffs2))

            }

        }
        //val chart1 = chartSelections(selectionsList1)
        //val chart2 = chartSelections(selectionsList2)

        val allRewards = totalRewards1.toList ++ totalRewards2.toList ++ totalRewards3.toList
        val chart = Chart(top=allRewards.max*1.1, bottom=allRewards.min*0.7, left=0, right=nIter, width = 150, height = 40)
        chart.plotLine(totalRewards1.toList, Some("1"), "+")
        chart.plotLine(totalRewards2.toList, Some("2"), "-")
        chart.plotLine(totalRewards3.toList, Some("3"), "~")
        println(chart.render())

        val lastSelections1 = selectionsList1.takeRight(1).flatMap(i => i); val lastSelections2 = selectionsList2.takeRight(1).flatMap(i => i); val lastSelections3 = selectionsList3.takeRight(1).flatMap(i => i)
        payoffs2.zipWithIndex.map{ case(payoff, i) => println(f"${strings(i/nModelsLevel2)+strings(i%nModelsLevel2)}: " + "%1.2f".format(payoff) + f" - ${lastSelections1(i)} - ${lastSelections2(i)} - ${lastSelections3(i)}  ")}

        //println(chart1)
        println("sum         " + "%1.2f".format(totalRewards1.takeRight(1)(0)) + " - " + "%1.2f".format(totalRewards2.takeRight(1)(0)) + " - " + "%1.2f".format(totalRewards3.takeRight(1)(0)))
        //println(chart2)

        println(ensemble3_1.export)
        println(ensemble3_2.export)

    }

    def calculateTotalReward(selections: List[Double], payoffs: List[Double]): Double = {
        selections.zip(payoffs).map{case(prob, payoff) => prob*payoff}.sum
    }


    def chartSelections(values: ListBuffer[List[Double]]): String = {
        val names = strings.slice(0, nModelsLevel1).flatMap(l1 => strings.slice(0, nModelsLevel2).map(l2 => l1 + l2) )
        val chars = List("+", "-", "~", "¬")
        val chart = Chart(top=1, bottom=0, left=0, right=nIter, width = 150, height = 40)
        (0 until values(0).length).map{i => 
            chart.plotLine(values(i), Some(names(i)), chars(math.min(4, i/(values(0).length/4))))
        }
        chart.render()
    }



}