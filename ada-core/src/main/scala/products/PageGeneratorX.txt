package ada.products

import scala.collection.mutable.ListBuffer
import scala.io.Source
import ada.ensembles.ThompsonSamplingEnsemble
import ada.models._
import ada.components.distributions.{Distribution, BetaDistribution}
import ada.interface.StackableEnsemble1
import ada.`package`._
import ada.assemblies.StackableAssembly1
import io.circe.Json

import ada.assemblies.MaskedStackableAssembly1
import ada.components.selectors.MaskedSelector
import cats.instances.option
import io.circe.{Decoder, Encoder}
import io.circe.DecodingFailure
import io.circe.HCursor
import ada.interface._
import breeze.stats.distributions.Beta

                                                                        //Map[ArticleID, List[List[Generatee]]]
class PageGeneratorX[Generatee](nFieldsI: Int, var candidatesX: Map[Int, List[List[Generatee]]])(
    implicit f: Generatee => Json,
            modelIdDecoder: Decoder[Int],
            modelActionEncoder: Encoder[Generatee],
            modelActionDecoder: Decoder[Generatee]){

    //json getSet
    //import ada.enhancements.GetSet; val getSet = new GetSet[ModelID,Unit,Generatee,BetaDistribution](); import getSet.{export, setParameters}

    type FieldID = Int
    type ArticleID = Int
    type ModelID = Int

    val generateeGeneratorAlpha = 1.0
    val generateeGeneratorBeta = 1.0
    val generateeGeneratorLearningRate = 0.4

    var (articleRewards, generateeRewards) = buildRewards(nFieldsI, candidatesX)
    var (assembly, ensembles, models): (MaskedStackableAssembly1[ArticleID,Unit,StackableAssembly1[ArticleID, Unit, Generatee, BetaDistribution],BetaDistribution], List[ThompsonSamplingEnsemble[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution]] with MaskedSelector[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution]]], Map[ArticleID,GenericStaticModel[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution],BetaDistribution]]) = buildModels(nFieldsI, candidatesX)

    //MAIN FUNCTIONS
    def generate(): List[(ArticleID, List[Generatee], List[Tree[ModelID]])] = {
        val (articles, articleIds): (Tree[StackableAssembly1[Int,Unit,Generatee, BetaDistribution]], Tree[ArticleID]) = assembly.actWithID((), new Stub() )
        val headlines: List[(Tree[Generatee], Tree[ModelID])] = articles match{
            case Branch(values) => values.map{ value => 
                value match {
                    case Leaf(value2) => value2.actWithID((), new Stub())
                    case Twig(value2, branches) => value2.actWithID((), new Stub())
                }
            }
        }
        val articleIds2 = articleIds match {
            case Branch(ids2) => ids2.map{
                ids3 => ids3 match{
                    case(Leaf(value)) => value
                }
            }
        }

        val headlines2 = headlines.map{
            case((generatee, modelIds)) => {
                ((convertTreeToListOfTrees(generatee), convertTreeToListOfTrees(modelIds)))
            }
        }
        articleIds2.zip(headlines2).map{case(articleId, (headline, modelIds)) => (articleId, headline.map(h => extractValuefromLeaf(h)), modelIds)}
    }

    def convertTreeToListOfTrees[A](tree: Tree[A]): List[Tree[A]] = {
        tree match {
                case Leaf(value) => List(Leaf(value))
                case Branch(values) => values
                case Twig(value, tree) => List()
                case Stub() => List()
        }
    }
    def extractValuefromLeaf[A](tree: Tree[A]): A  = {
        tree match {
            case Leaf(value) => value
            case _ => throw new Exception("Not a Leaf!!")
        }
    }

    def update(selectedArticle: Option[ArticleID], headlines: List[(ArticleID, List[Tree[ModelID]])]): Unit = {
        //temporary
        val transformedHeadlines = headlines.map{case(((articleId, modelIds_))) => (articleId, new Branch(modelIds_.zipWithIndex.map{case(modelIds_2, i) => modelIds_2}.toList))}.toList
        val articleIds = new Branch((0 until nFieldsI).zip(transformedHeadlines).map{case(i, (articleId, modelIds_)) => new Twig(articleId, modelIds_)}.toList)
        require(headlines.length == nFieldsI, "headlines length is: " + headlines.length.toString)

        transformedHeadlines.map{case(articleId, headlineIds) => models(articleId).getValue.update(headlineIds, (), new Reward(0.5 - 0.1/nFieldsI))}

        selectedArticle match {
            case None => assembly.update(articleIds, (), new Reward(0.0))
            case Some(articleId) => {
                //update assembly on article selection - reward for any article being selected
                assembly.update(articleIds, (), new Reward(1.0))
                //assembly.update((0 until nFields).map(i => List(articleId)).toList, (), new Reward(1.0))
                //update 
                models(articleId).value.update(transformedHeadlines.filter(_._1 == articleId)(0)._2, (), new Reward(1.0))
            }
        }
    }

    //INTERNAL
    def initialiseRewards(modelIds: List[Int]): Map[Int, BetaDistribution] = modelIds.map{id => (id, new BetaDistribution(generateeGeneratorAlpha, generateeGeneratorBeta, generateeGeneratorLearningRate))}.toMap
    //replace ThompsonSamplingEnsemble[Int, Unit, String] with StackableAssembly(List(ThompsonSamplingEnsemble[Int, Unit, PhotoID], ThompsonSamplingEnsemble[Int, Unit, ModelID], ThompsonSamplingEnsemble[Int, Unit, TeaserID], ThompsonSamplingEnsemble[Int, Unit, Text]))

    def buildRewards(nFields: Int, candidatesX: Map[ArticleID, List[List[Generatee]]]): (Map[FieldID,Map[ArticleID,BetaDistribution]], Map[ArticleID,List[Map[ModelID,BetaDistribution]]]) = {
        val generateeRewards = candidatesX.map{case(k, candidates) => (k, candidates.map{candidates2 => initialiseRewards((0 until candidates2.length).toList)})}.toMap
        val articleRewards = (0 until nFields).map{i => (i, initialiseRewards(candidatesX.keys.toList))}.toMap
        (articleRewards, generateeRewards)
    }

    def listOfMapsToMapOfLists[A, B](list: List[Map[A, B]]): Map[A, List[B]] = list.flatten.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    //val models1 = (0 until 5).map(i => (i, new ThompsonSamplingEnsemble[Int, Unit, String](Map(0 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i).toString), 1 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i+5).toString)), initialiseRewards(List(0, 1)) )))
    //val models2 = (0 until 5).map(i => (i+1, new ThompsonSamplingEnsemble[Int, Unit, String](Map(0 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i+10).toString), 1 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i+15).toString)), initialiseRewards(List(0, 1)) )))
    //val assemblies = (0 until 5).map(i => new StackableAssembly1[Int, Unit, String, BetaDistribution](List(models1(i), models2(i))))
    //val assembly = new StackableAssembly1(assemblies.zipWithIndex.map{case(a, i) => (i, a)}.toList)
    
    def buildModels(nFields: Int, candidatesX: Map[ArticleID, List[List[Generatee]]]): (MaskedStackableAssembly1[ArticleID,Unit,StackableAssembly1[ArticleID, Unit, Generatee, BetaDistribution],BetaDistribution], List[ThompsonSamplingEnsemble[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution]] with MaskedSelector[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution]]], Map[ArticleID,GenericStaticModel[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution],BetaDistribution]]) = {
        val models: Map[ArticleID,GenericStaticModel[ArticleID,Unit,StackableAssembly1[ModelID,Unit,Generatee, BetaDistribution],BetaDistribution]] = candidatesX.map{case(candidateId, candidates) =>
            (candidateId, new GenericStaticModel[ArticleID, Unit, StackableAssembly1[ArticleID, Unit, Generatee, BetaDistribution], BetaDistribution](
                        new StackableAssembly1(
                            candidates.zipWithIndex.map{case(candidates2, i) => ThompsonSamplingEnsemble[ModelID, Unit, Generatee](
                                                            candidates2.zipWithIndex.map{case(generatee, i2) => (i2, new GenericStaticModel[ModelID, Unit, Generatee, BetaDistribution](generatee))}.toMap,
                                                            generateeRewards(candidateId)(i))
                        }
                        )))
        }.toMap
        val ensembles = (0 until nFields).map{i =>
            new ThompsonSamplingEnsemble[ArticleID, Unit, StackableAssembly1[ModelID, Unit, Generatee, BetaDistribution]](models, articleRewards(i), 0.0)
                                                        with MaskedSelector[ArticleID, Unit, StackableAssembly1[ModelID, Unit, Generatee, BetaDistribution]]
        }.toList

        //implicit val intDecoder = Decoder[Int]
        val assembly = new MaskedStackableAssembly1[ArticleID, Unit, StackableAssembly1[ModelID, Unit, Generatee, BetaDistribution], BetaDistribution](ensembles)
        (assembly, ensembles, models)
    }

    
    //GET SET
    val firstKey = candidatesX.keys.toList(0)
    import ada.enhancements.GetSetEnsemble; val getSetEnsemble = new GetSetEnsemble[ModelID, Unit, Generatee, BetaDistribution](); import getSetEnsemble.buildExportEnsembleParameters
    implicit val (ensembleEncoder, ensembleDecoder) = buildExportEnsembleParameters[ThompsonSamplingEnsemble[ModelID, Unit, Generatee]]()
    implicit val modelEncoder: Encoder[StackableModel[ModelID, Unit, Generatee]] = new Encoder[StackableModel[ModelID, Unit, Generatee]]{
        final def apply(model: StackableModel[ModelID, Unit, Generatee]): Json = {
            model match {
                case(m: ThompsonSamplingEnsemble[ModelID, Unit, Generatee]) => ensembleEncoder(m)
                case _ => throw new Exception("Only ThompsonSamplingEnsemble can be decoded")
            }
        }
    }
    implicit val modelDecoder: Decoder[StackableModel[ModelID, Unit, Generatee]] = new Decoder[StackableModel[ModelID, Unit, Generatee]]{
        final def apply(c: HCursor): Decoder.Result[StackableModel[ModelID, Unit, Generatee]] = {
            ensembleDecoder(c)
        }
    }
                                            
    import ada.enhancements.GetSetAssembly; val getSetAssembly = new GetSetAssembly[ModelID, Unit, Generatee, BetaDistribution]()
    implicit val (assemblyEncoder, assemblyDecoder) = getSetAssembly.buildExportAssemblyParameters()

    //val getSetEnsemble2 = new GetSetEnsemble[ModelID, Unit, StackableAssembly1[ModelID, Unit, Generatee, BetaDistribution], BetaDistribution]();
    //val (ensembleEncoder2, ensembleDecoder2) = getSetEnsemble2.buildExportEnsembleParameters[ThompsonSamplingEnsemble[ModelID, Unit, StackableAssembly1[ModelID, Unit, Generatee, BetaDistribution]]](ensembles(0)._2)

    import ada.enhancements.GetSetAssembly; val getSetAssembly2 = new GetSetAssembly[ArticleID, Unit, StackableAssembly1[ModelID, Unit, Generatee, BetaDistribution], BetaDistribution](); import getSetAssembly2.{setAssemblyParameters, getAssemblyParameters}

    def export(): Json = getAssemblyParameters(assembly)

    def set(parameters: Json): Unit = setAssemblyParameters(assembly, parameters)
    
    //REINITIALISATION
    def reinitialise(nFields: Int, candidatesX: Map[ArticleID, List[List[Generatee]]]): Unit = {
        buildSetRewards(nFields, candidatesX)
        buildSetModels(nFields, candidatesX)
    }
    def buildSetRewards(nFields: Int, candidatesX: Map[ArticleID, List[List[Generatee]]]): Unit = {
        val tuple = buildRewards(nFields, candidatesX)
        articleRewards = tuple._1; generateeRewards = tuple._2
    }
    def buildSetModels(nFields: Int, candidatesX: Map[ArticleID, List[List[Generatee]]]): Unit = {
        val tuple = buildModels(nFields, candidatesX)
        assembly = tuple._1; ensembles = tuple._2; models = tuple._3
    }

    //MUTATING REWARDS
    def addHeadlineReward(articleId: ArticleID, generateeDepth: Int, reward: BetaDistribution): Unit = {
        generateeRewards = generateeRewards.map{
            case(id, list) if id == articleId => (id, list.zipWithIndex.map{
                case(rewards, depth) if depth == generateeDepth  => rewards ++ Map(rewards.toList.length+1 -> reward)
                case(other2) => other2._1
            })
            case(other) => other
        }.toMap
    }
    def addArticleReward(articleId: ArticleID, articleReward: BetaDistribution, headlineRewards: List[List[BetaDistribution]]): Unit = {
        generateeRewards = generateeRewards ++ Map(articleId -> headlineRewards.map{headlineRewards2 => (0 until headlineRewards2.length).map{i => (i, headlineRewards2(i))}.toMap}.toList)
        articleRewards = articleRewards.map{case(fieldId, map) => (fieldId, map ++ Map(articleId -> articleReward))}.toMap
    }
    def removeArticleReward(articleId: ArticleID): Unit = {
        generateeRewards = generateeRewards.filter{case(id, rewards) => id != articleId}
        articleRewards = articleRewards.map{case(fieldId, articleReward) => (fieldId, articleReward.filter{case(id, _) => id != articleId} )}.toMap
    }

    def removeHeadlineReward(articleId: ArticleID, generateeDepth: Int, generateeId: ModelID): Unit = {
        generateeRewards = generateeRewards.map{
                case(id, list) if id == articleId => (id, list.zipWithIndex.map{
                    case(l, depth) if depth == generateeDepth => l.filter{case(id2, rewards) => generateeId != id2}
                    case(l, _) => l
                })
                case(x) => x  
        }
    }
    //ADDING DROPPING
    //adding headline through reinitialisation - mutation would require mutable Maps
    def addHeadline(articleId: ArticleID, generateeDepth: Int, generatee: Generatee, reward: BetaDistribution): Unit = {
        if(candidatesX.keys.map(id => id == articleId).max == true) {
            candidatesX = candidatesX.map{
                case(id, list) if id == articleId => {
                    addHeadlineReward(articleId, generateeDepth, reward)
                    (id, list.zipWithIndex.map{
                        case(l, depth) if depth == generateeDepth => l ++ List(generatee)
                        case(l, _) => l
                    })
                }
                case(id, list) => (id, list)
            }.toMap
            reinitialise(nFieldsI, candidatesX)
        } else{
            throw new Exception(s"$articleId cannot be found")
        }
    }

    def addArticle(articleId: ArticleID, candidate: List[List[Generatee]], articleReward: BetaDistribution, headlineRewards: List[List[BetaDistribution]]): Unit = {
        require(candidate.length == headlineRewards.length, "list of generatees and list of generatee rewards must be of the same length")
        candidatesX.keys.map{id => if (id == articleId) throw new Exception(articleId.toString + " already exists.")}
        candidatesX = candidatesX ++ Map(articleId -> candidate)
        addArticleReward(articleId, articleReward, headlineRewards)
        reinitialise(nFieldsI, candidatesX)
    }

    def dropArticle(articleId: ArticleID): Unit = {
        if(candidatesX.contains(articleId)){
            removeArticleReward(articleId)
            candidatesX =  candidatesX.filter{case(id, list) => id != articleId}.toMap
            reinitialise(nFieldsI, candidatesX)
        } else {
            throw new Exception(s"$articleId cannot be found")
        }
    }

    def filterOnHeadline(articleId: ArticleID, generateeDepth: Int, generatee: Generatee)(implicit identityGeneratees: (Generatee, Generatee) => Boolean): Unit = {
        if(candidatesX.keys.map(id => id == articleId).max == true) {
            candidatesX = candidatesX.map{
                case(id, list) if id == articleId => {
                    (id, list.zipWithIndex.map{
                        case(list2, depth) if depth == generateeDepth => {
                            val (targetId, list3) = filterAndCompare(list2, generatee)
                            removeHeadlineReward(articleId, generateeDepth, targetId)
                            list2
                        }
                        case(list2, _) => list2
                    })
                }
                case(id, list) => (id, list)
            }.toMap
            reinitialise(nFieldsI, candidatesX)
        } else{
            throw new Exception(s"$articleId cannot be found")
        }
        def filterAndCompare(list1: List[Generatee], generatee: Generatee): (ModelID, List[Generatee]) = {
            val (target, list) = list1.zipWithIndex.partition{case(gen,i) => identityGeneratees(gen, generatee)} 
            val targetId: Int = target match {
                case(Nil) => throw new Exception(articleId.toString + " does not contain " + generatee.toString())
                case(h1 :: h2 :: t1 :: t2) =>  throw new Exception(articleId.toString + " contains two identical generatees")
                case(h :: x) => h._2 
            }
            (targetId, list.map(_._1))
        }
    }

    def dropHeadline(articleId: ArticleID, generateeDepth: Int, generatee: Generatee)(implicit identityGeneratees: (Generatee, Generatee) => Boolean): Unit = {
        filterOnHeadline(articleId, generateeDepth, generatee)
    }
    def dropAllButHeadline(articleId: ArticleID, generateeDepth: Int, generatee: Generatee)(implicit identityGeneratees: (Generatee, Generatee) => Boolean): Unit = {
        filterOnHeadline(articleId, generateeDepth, generatee)((g1, g2) => identityGeneratees(g1, g2) == false)
    }

    //Resetting rewards
    def resetRewards(): Unit = {
        val tuple = buildRewards(nFieldsI, candidatesX)
        articleRewards = tuple._1; generateeRewards = tuple._2
    }
    
}


