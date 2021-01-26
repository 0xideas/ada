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
import ada.`package`._

object CoordinationLearning{
    val nModelsLevel1 = 10 //the number of models on the first level of the stacked ensemble
    val nModelsLevel2 = 10 // the number on the second level
    val learningRate = 0.05 // the actual update of the Beta Distribution coefficient at rewards 1.0 and 0.0

    val changePayoffs = 0.5 //position in the iterations where payoffs are reinitialised with a negative -Multiplier
    val level1Factor: Double = 0.0 //hyperparameter to control the degree of correlation between level 1 model and reward
    val level2Factor: Double = 0.0 //hyperparameter to control the degree of correlation between level 2 model and reward
    val driftValues = true
    val driftInterval: Option[Int] = Some(2000)
    val driftFactor: Double = 1.0
    val minIter = 1000 //minimum number of iterations
    val maxIter = 10000 //maximum number of iterations



    val nIter = math.max(math.min(100*nModelsLevel1*nModelsLevel2,maxIter), minIter)
    val strings = List.fill(100)(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")).flatMap(i => i)
    val r = new scala.util.Random
    val iMultiplierI = level1Factor * 100/(nModelsLevel1-1)
    val jMultiplierI = level2Factor * 100/(nModelsLevel2-1)

    val maxV = 100 + iMultiplierI*(nModelsLevel1-1) + jMultiplierI*(nModelsLevel2-1)
    var (payoffs1, payoffs2) = initialise_payoffs(iMultiplierI, jMultiplierI, maxV)


    val models1 = (0 until nModelsLevel1*nModelsLevel2).map(i =>  new StaticModelString[Int, Unit, String](strings(i/nModelsLevel2)+strings(i%nModelsLevel2)))
    val ensembles1 = (0 until nModelsLevel1).map(i => ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel2).zip(models1.slice(i*nModelsLevel2, (i+1)*nModelsLevel2)).toMap, 1, 1, learningRate))
    val ensemble1 = ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel1).zip(ensembles1).toMap, 1, 1, learningRate)
    
    val models2 = (0 until nModelsLevel1*nModelsLevel2).map(i =>  new StaticModelString[Int, Unit, String](strings(i/nModelsLevel1)+strings(i%nModelsLevel1)))
    val ensemble2 = ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel1*nModelsLevel2).zip(models2).toMap, 1, 1, learningRate)

    val models3_1 = (0 until nModelsLevel1).map(i =>  new StaticModelString[Int, Unit, String](strings(i)))
    val ensemble3_1 = ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel1).zip(models3_1).toMap, 1, 1, learningRate)
    val models3_2 = (0 until nModelsLevel2).map(i =>  new StaticModelString[Int, Unit, String](strings(i)))
    val ensemble3_2 = ThompsonSamplingEnsemble[Int, Unit, String]((0 until nModelsLevel2).zip(models3_2).toMap, 1, 1, learningRate)

    val selectionsList1 = ListBuffer[List[Double]](); val selectionsList2 = ListBuffer[List[Double]](); val selectionsList3 = ListBuffer[List[Double]]()
    val totalRewards1 = ListBuffer[Double](); val totalRewards2 = ListBuffer[Double](); val totalRewards3 = ListBuffer[Double]()


    def run(): Unit = {
        (0 until nIter).map{i => 
            val (action1, modelIds1) = ensemble1.actWithID((), List())
            val (action2, modelIds2) = ensemble2.actWithID((), List())

            val (action3_1, modelIds3_1) = ensemble3_1.actWithID((), List())
            val (action3_2, modelIds3_2) = ensemble3_2.actWithID((), List())
            val (action3, modelIds3) = (action3_1 + action3_2, modelIds3_1 ++ modelIds3_2)

            val driftInterval2: Int = driftInterval match {
                case None => nIter/2
                case Some(interval) => math.max(interval, 1)
            }

            ensemble1.update(modelIds1, (), new Reward(payoffs1(modelIds1(0))(modelIds1(1))))
            ensemble2.update(modelIds2, (), new Reward(payoffs2(modelIds2(0))))

            val reward3 = new Reward(payoffs1(modelIds3(0))(modelIds3(1)))
            ensemble3_1.update(modelIds3_1, (), reward3 )
            ensemble3_2.update(modelIds3_2, (), reward3)
            //println( f"ensemble 3 : ${action3} reward ${reward3}")

            //println(f"${i}:   ${action1}   ${action2}")
            if(i % math.max((nIter/100), 1) == 0){
                val nIterMeasure = math.min(10*nModelsLevel1*nModelsLevel2, 100)
                val selections1 = Utilities.averageSelectedModelsLevel2(Utilities.selectStackable[Unit, String, BetaDistribution](ensemble1, (), 5, nIterMeasure), nModelsLevel1, nModelsLevel2)
                val selections2 = Utilities.selectAndAverageStackable[Unit, String, BetaDistribution](ensemble2, (), nModelsLevel1*nModelsLevel2, nIterMeasure)
                val selections3 = Utilities.averageSelectedModelsLevel2(Utilities.selectAndAverageStackableFromTwo[Unit, String, BetaDistribution](ensemble3_1, ensemble3_2, (), nModelsLevel1*nModelsLevel2, nIterMeasure), nModelsLevel1, nModelsLevel2)

                selectionsList1.append(selections1); selectionsList2.append(selections2); selectionsList3.append(selections3)
                totalRewards1.append(calculateTotalReward(selections1, payoffs2)); totalRewards2.append(calculateTotalReward(selections2, payoffs2)); totalRewards3.append(calculateTotalReward(selections3, payoffs2))
                //println(f"iteration: ${i} total " + "%1.2f".format(totalRewards1.sum /totalRewards1.length) + " - " + "%1.2f".format(totalRewards2.sum/totalRewards2.length) + " - " + "%1.2f".format(totalRewards3.sum/totalRewards3.length))
                println(f"iteration: ${i} - ${payoffs2.reverse.slice(0,10).map(p => "%1.2f".format(p)).mkString("-")}")
            }
            if(driftValues && i > 0 && i % driftInterval2 == 0){
                val (newPayoffs1, newPayoffs2): (List[List[Double]], List[Double]) = initialise_payoffs(iMultiplierI, jMultiplierI, maxV, driftFactor, Some(payoffs1))
                if(driftFactor == 1.0){
                    val (newPayoffs1, newPayoffs2): (List[List[Double]], List[Double]) = initialise_payoffs(iMultiplierI, jMultiplierI, maxV, payoffsBase=None)
                }
                payoffs1 = newPayoffs1
                payoffs2 = newPayoffs2
            }


        }
        //val chart1 = chartSelections(selectionsList1)
        //val chart2 = chartSelections(selectionsList2)

        val allRewards = totalRewards1.toList ++ totalRewards2.toList ++ totalRewards3.toList
        val chart = Chart(top=allRewards.max*1.1, bottom=allRewards.min*0.7, left=0, right=nIter, width = 150, height = 40)
        chart.plotLine(totalRewards1.toList, Some("1"), "-")
        chart.plotLine(totalRewards2.toList, Some("2"), "~")
        chart.plotLine(totalRewards3.toList, Some("3"), "+")
        println(chart.render())
        
        
        val nIterMeasure = math.min(10*nModelsLevel1*nModelsLevel2, 1000)
        val selections1 = Utilities.averageSelectedModelsLevel2(Utilities.selectStackable[Unit, String, BetaDistribution](ensemble1, (), 5, nIterMeasure), nModelsLevel1, nModelsLevel2)
        val selections2 = Utilities.selectAndAverageStackable[Unit, String, BetaDistribution](ensemble2, (), nModelsLevel1*nModelsLevel2, nIterMeasure)
        val selections3 = Utilities.averageSelectedModelsLevel2(Utilities.selectAndAverageStackableFromTwo[Unit, String, BetaDistribution](ensemble3_1, ensemble3_2, (), nModelsLevel1*nModelsLevel2, nIterMeasure), nModelsLevel1, nModelsLevel2)

        //payoffs2.zipWithIndex.map{ case(payoff, i) => println(f"${strings(i/nModelsLevel2)+strings(i%nModelsLevel2)}: " + "%1.2f".format(payoff) + f" - ${selections1(i)} - ${selections2(i)} - ${selections3(i)}  ")}
        println(f"${payoffs2.min} - ${payoffs2.sorted.slice(payoffs2.length/2-1, payoffs2.length/2)(0)} ${payoffs2.max}")
        //println(chart1)
        println("last        " + "%1.2f".format(calculateTotalReward(selections1, payoffs2)) + " - " + "%1.2f".format(calculateTotalReward(selections2, payoffs2)) + " - " + "%1.2f".format(calculateTotalReward(selections3, payoffs2)))
        println("total      " + "%1.2f".format(totalRewards1.sum/totalRewards1.length) + " - " + "%1.2f".format(totalRewards2.sum/totalRewards2.length) + " - " + "%1.2f".format(totalRewards3.sum/totalRewards3.length))
        //println(chart2)

        //println(ensemble3_1.export)
        //println(ensemble3_2.export)

    }

    def initialise_payoffs(iMultiplier: Double, jMultiplier: Double, maxV: Double, driftFactor: Double = 0.0,  payoffsBase: Option[List[List[Double]]] = None): (List[List[Double]], List[Double]) = {
        val payoffs1base = payoffsBase match{
            case(None) => (0 until nModelsLevel1).map(i => (0 until nModelsLevel2).map(j =>  ( r.nextInt(100).toDouble + i.toDouble*iMultiplier + j.toDouble*jMultiplier)/maxV).toList).toList
            case(Some(payoffsBase_)) => payoffsBase_.zipWithIndex.map{case(payoffs,i) => payoffs.zipWithIndex.map{case(payoff, j) => math.min(payoff+0.01, payoff +  (r.nextInt(3)-1)*driftFactor*0.5*(payoff+ r.nextDouble())) }}
        }
        val min = payoffs1base.flatMap(i => i).min
        val max = payoffs1base.flatMap(i => i).map(_-min).max
        val payoffs1 = payoffs1base.map(payoffs => payoffs.map(payoff => (payoff-min)/max))
        val payoffs2 = payoffs1.flatMap(i => i)
        (payoffs1, payoffs2)
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