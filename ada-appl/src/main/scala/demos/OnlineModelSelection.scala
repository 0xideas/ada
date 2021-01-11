package ada.demos

import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.core.ensembles.PassiveThompsonSamplingEnsemble
import ada.core.ensembles.ThompsonSamplingLocalBeta
import ada.core.models._
import ada.core.components.distributions.{Distribution, BetaDistribution}
import scala.xml.persistent.Index

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption

class OnlineModelSelection{
    val data_path = "/home/leon/data/onnx/ada-example/data_narrow.txt"
    val labels_path = "/home/leon/data/onnx/ada-example/labels_narrow.txt"
    val fine_labels_path = "/home/leon/data/onnx/ada-example/fine_labels_narrow.txt"

    val models = (101 until 106).map{i => 
        new OnnxClassifier[Int, Array[Array[Float]], Int](f"/home/leon/data/onnx/ada-example/conv-seed${i}.onnx", "input", i => i)
    }

    def run(): Unit = {
        //measure(List(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000))
        measureRecall()
    }

    def measure(iterList: List[Int]){
        if(Files.exists(Paths.get(f"/home/leon/projects/ada-scratchpad/ts-${iterList(0)}.txt")) == false){
            iterList.map{ iter => Files.write(Paths.get(f"/home/leon/projects/ada-scratchpad/ts-${iter}.txt"), "".getBytes(StandardCharsets.UTF_8))}
        }

        val selections = trainEnsemble(iterList)
        iterList.zipWithIndex.map{
            case(iter, i) => Files.write(Paths.get(f"/home/leon/projects/ada-scratchpad/ts-${iter}.txt"), ("\n" + selections(i).mkString(",")).getBytes(StandardCharsets.UTF_8),  StandardOpenOption.APPEND)
        }

    }

    def trainEnsemble(nIterL: List[Int] = List(1000)): List[List[Double]] = {
        val ensemble = new ThompsonSamplingLocalBeta[Int, Array[Array[Array[Array[Float]]]], Int]((0 until 5).zip(models).toMap, 1, 1)

        val (labels, data) = loadData()
        val rnd = util.Random
        val n = data.length

        val selectionsList = nIterL.map{nIter => 
            (0 until nIter).map{i =>
                val pick = math.abs(rnd.nextInt % n)
                val (action, modelIds) = ensemble.actWithID(data(pick), List())
                val reward =  if(action == labels(pick)) 1.0 else 0.0
                ensemble.update(modelIds, reward)

            }

            val selections = Utilities.selectAndAverageNoContext[Array[Array[Array[Array[Float]]]], Int, BetaDistribution](ensemble, data(0), 5, 1000)
            println(selections)
            selections
        }
        
        selectionsList.toList.map(_.toList)
        //println(ensemble.export)

    }

    def loadData(): (ListBuffer[Int], ListBuffer[Array[Array[Array[Array[Float]]]]]) = {
        val lines = Source.fromFile(data_path).getLines
        val labelslines = Source.fromFile(labels_path).getLines
        val finelabelslines = Source.fromFile(fine_labels_path).getLines

        val labels = ListBuffer[Int]()
        val datas =  ListBuffer[Array[Array[Array[Array[Float]]]]]()
        while(lines.hasNext){
            val raw = lines.next.split(",").map(_.toInt)
            val data = Array(raw.grouped(1024).toArray.map(rr => rr.grouped(32).toArray.map(rrr => rrr.map(rrrr => rrrr.toFloat))))
            datas.append(data)
            labels.append(labelslines.next.toInt)
        }
        (labels, datas)
    }


    def measureRecall(): Unit = {
        val (predictions, labels, fine_labels) = applyModels(data_path, labels_path, fine_labels_path, threshold = 1)

        val correct = predictions.zipWithIndex.map{ case(preds, i) => {
            preds.map(pred => if(pred == labels(i)) 1.0 else 0.0)
        }}
        println("accuracies: " + calcAccuracy(correct, predictions.length))

        val c = correct.zipWithIndex.groupBy{
            case(preds, i) => fine_labels(i)
        }.mapValues(v => v.map(_._1))
        
        c.map{
            case(fine_label, accur) => println(f"${fine_label} recall: " + calcAccuracy(accur, accur.length))
        }
    }

    def calcAccuracy(array: ListBuffer[List[Double]], n:Int): String = 
        array.transpose.map(_.sum).foldLeft("")( (acc, accur) => acc + f" ${accur/n}%2.2f")

    def applyModels(data_path: String, labels_path: String, fine_labels_path: String, threshold: Double = 0.001, silent: Boolean = true): (ListBuffer[List[Int]], ListBuffer[Int],  ListBuffer[Int]) = {
        val rnd = util.Random
        val lines = Source.fromFile(data_path).getLines
        val labelslines = Source.fromFile(labels_path).getLines
        val finelabelslines = Source.fromFile(fine_labels_path).getLines


        val predictionss = ListBuffer[List[Int]]()
        val labels = ListBuffer[Int]()
        val fine_labels = ListBuffer[Int]()
        var i = 0
        while(lines.hasNext){
            if(rnd.nextFloat() <  threshold){
                print(f"${i} ")
                val raw = lines.next.split(",").map(_.toInt)
                val data = Array(raw.grouped(1024).toArray.map(rr => rr.grouped(32).toArray.map(rrr => rrr.map(rrrr => rrrr.toFloat))))
                val label = labelslines.next.toInt
                val predictions = models.map(model => model.act(data)).toList
                if(silent == false) print_predictions(label, predictions)

                labels.append(label)
                predictionss.append(predictions)
                fine_labels.append(finelabelslines.next.toInt)
            } else {
                lines.next
                labelslines.next
            }
            i += 1
        }
        //println("\u001b[2J")
        (predictionss, labels, fine_labels)
    } 

    def print_predictions(label: Int, predictions: List[Int]): Unit = {
        print(f"target: ${label}")
        print("\tpredictions: ")
        predictions.map(pred => print(f" ${pred}"))
        println()
    }
}