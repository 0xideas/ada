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

                                                                        //Map[ArticleID, List[Generatee]]
class PageGenerator[Generatee](nFieldsI: Int, private var candidatesI: Map[Int, List[Generatee]])(
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

    var (articleRewards, generateeRewards) = buildRewards(nFieldsI, candidatesI)
    implicit var (assembly, ensembles, models) = buildModels(nFieldsI, candidatesI)
    
    //MAIN FUNCTIONS
    def generate(): List[(ArticleID, Generatee, Tree[ModelID])] = {
        val (articles, articleIds): (Tree[ThompsonSamplingEnsemble[Int,Unit,Generatee]], Tree[ArticleID]) = assembly.actWithID((), new Stub() )
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
                val gen = generatee match {
                    case Leaf(value) => value
                }
                ((gen, modelIds))
            }
        }
        articleIds2.zip(headlines2).map{case(articleId, (headline, modelIds)) => (articleId, headline, modelIds)}
    }

    def update(selectedArticle: Option[ArticleID], headlines: List[(ArticleID, Tree[ModelID])]): Unit = {
        //temporary
        val articleIds = new Branch((0 until nFieldsI).zip(headlines).map{case(i, (articleId, modelIds_)) => new Twig(articleId, modelIds_)}.toList)

        require(headlines.length == nFieldsI, "headlines length is: " + headlines.length.toString)

        headlines.map{case(articleId, headlineIds) => models(articleId).getValue.update(headlineIds, (), new Reward(0.5 - 0.1/nFieldsI))}

        selectedArticle match {
            case None => assembly.update(articleIds, (), new Reward(0.0))
            case Some(articleId) => {
                //update assembly on article selection - reward for any article being selected
                assembly.update(articleIds, (), new Reward(1.0))
                //assembly.update((0 until nFields).map(i => List(articleId)).toList, (), new Reward(1.0))
                //update
                models(articleId).value.update(headlines.filter(_._1 == articleId)(0)._2, (), new Reward(1.0))
            }
        }
    }

    //INTERNAL
    private def initialiseRewards(modelIds: List[Int]): Map[Int, BetaDistribution] = modelIds.map{id => (id, new BetaDistribution(generateeGeneratorAlpha, generateeGeneratorBeta, generateeGeneratorLearningRate))}.toMap
    //replace ThompsonSamplingEnsemble[Int, Unit, String] with StackableAssembly(List(ThompsonSamplingEnsemble[Int, Unit, PhotoID], ThompsonSamplingEnsemble[Int, Unit, ModelID], ThompsonSamplingEnsemble[Int, Unit, TeaserID], ThompsonSamplingEnsemble[Int, Unit, Text]))

    private def buildRewards(nFields: Int, candidates: Map[ArticleID, List[Generatee]]): (Map[FieldID,Map[ArticleID,BetaDistribution]], Map[ArticleID,Map[ModelID,BetaDistribution]]) = {
        val generateeRewards = candidates.map{case(k, cand) => (k, initialiseRewards((0 until cand.length).toList))}.toMap
        val articleRewards = (0 until nFields).map{i => (i, initialiseRewards(candidates.keys.toList))}.toMap
        (articleRewards, generateeRewards)
    }

    //val models1 = (0 until 5).map(i => (i, new ThompsonSamplingEnsemble[Int, Unit, String](Map(0 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i).toString), 1 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i+5).toString)), initialiseRewards(List(0, 1)) )))
    //val models2 = (0 until 5).map(i => (i+1, new ThompsonSamplingEnsemble[Int, Unit, String](Map(0 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i+10).toString), 1 -> new GenericStaticModel[Int, Unit, String, BetaDistribution](letters(i+15).toString)), initialiseRewards(List(0, 1)) )))
    //val assemblies = (0 until 5).map(i => new StackableAssembly1[Int, Unit, String, BetaDistribution](List(models1(i), models2(i))))
    //val assembly = new StackableAssembly1(assemblies.zipWithIndex.map{case(a, i) => (i, a)}.toList)
    
    private def buildModels(nFields: Int, candidates: Map[ArticleID, List[Generatee]]): (MaskedStackableAssembly1[ArticleID,Unit,ThompsonSamplingEnsemble[ModelID,Unit,Generatee],BetaDistribution], List[ThompsonSamplingEnsemble[ArticleID,Unit,ThompsonSamplingEnsemble[ModelID,Unit,Generatee]] with MaskedSelector[ArticleID,Unit,ThompsonSamplingEnsemble[ModelID,Unit,Generatee]]], Map[ArticleID,GenericStaticModel[ArticleID,Unit,ThompsonSamplingEnsemble[ModelID,Unit,Generatee],BetaDistribution]]) = {
        val models = candidates.keys.map(k => (k, new GenericStaticModel[ArticleID, Unit, ThompsonSamplingEnsemble[ModelID, Unit, Generatee], BetaDistribution](
                                                            ThompsonSamplingEnsemble[ModelID, Unit, Generatee](
                                                                candidates(k).zipWithIndex.map{case(generatee, k2) => (k2, new GenericStaticModel[ModelID, Unit, Generatee, BetaDistribution](generatee))}.toMap,
                                                                generateeRewards(k))
                                                            ))
                                                        ).toMap
                                                        
        val ensembles = (0 until nFields).map{i =>
            new ThompsonSamplingEnsemble[ArticleID, Unit, ThompsonSamplingEnsemble[ModelID, Unit, Generatee]](models, articleRewards(i), 0.0)
                                                        with MaskedSelector[ArticleID, Unit, ThompsonSamplingEnsemble[ModelID, Unit, Generatee]]
        }.toList

        //implicit val intDecoder = Decoder[Int]
        val assembly = new MaskedStackableAssembly1[ArticleID, Unit, ThompsonSamplingEnsemble[ModelID, Unit, Generatee], BetaDistribution](ensembles)
        (assembly, ensembles, models)
    }

    
    //GET SET
    val firstKey = candidatesI.keys.toList(0)
    import ada.enhancements.GetSetEnsemble; val getSetEnsemble = new GetSetEnsemble[ModelID, Unit, Generatee, BetaDistribution](); import getSetEnsemble.buildExportEnsembleParameters
    implicit val (ensembleEncoder, ensembleDecoder) = buildExportEnsembleParameters[ThompsonSamplingEnsemble[ModelID, Unit, Generatee]]                              
    val getSetEnsemble1 = new GetSetEnsemble[ModelID, Unit, ThompsonSamplingEnsemble[ModelID, Unit, Generatee], BetaDistribution](); import getSetEnsemble1.{buildExportEnsembleParameters => buildExportEnsembleParameters1} 
    implicit val (ensembleEncoder1, ensembleDecoder1) = buildExportEnsembleParameters1

    import ada.enhancements.GetSetAssembly; val getSetAssembly = new GetSetAssembly[ArticleID, Unit, ThompsonSamplingEnsemble[ModelID, Unit, Generatee], BetaDistribution](); import getSetAssembly._

    def export(): Json = getAssemblyParameters(assembly)

    def set(parameters: Json): Unit = setAssemblyParameters(assembly, parameters)


    
    //REINITIALISATION
    def reinitialise(nFieldsS: Int, candidatesS: Map[ArticleID, List[Generatee]]): Unit = {
        buildSetRewards(nFieldsS, candidatesS)
        buildSetModels(nFieldsS, candidatesS)
    }
    def buildSetRewards(nFields: Int, candidates: Map[ArticleID, List[Generatee]]): Unit = {
        val tuple = buildRewards(nFields, candidates)
        articleRewards = tuple._1; generateeRewards = tuple._2
    }
    def buildSetModels(nFields: Int, candidates: Map[ArticleID, List[Generatee]]): Unit = {
        val tuple = buildModels(nFields, candidates)
        assembly = tuple._1; ensembles = tuple._2; models = tuple._3
    }

    //MUTATING REWARDS - NO IDEA IF THEY WORK
    def addHeadlineReward(articleId: ArticleID, reward: BetaDistribution): Unit = {
        generateeRewards = generateeRewards.map{
            case(id, map) if id == articleId => (articleId, map ++ Map(map.toList.length+1 -> reward))
            case(other) => other
        }.toMap
    }
    def addArticleReward(articleId: ArticleID, articleReward: BetaDistribution, headlineRewards: List[BetaDistribution]): Unit = {
        generateeRewards = generateeRewards ++ Map(articleId -> (0 until headlineRewards.length).map{i => (i, headlineRewards(i))}.toMap)
        articleRewards = articleRewards.map{case(fieldId, map) => (fieldId, map ++ Map(articleId -> articleReward))}.toMap
    }
    def removeArticleReward(articleId: ArticleID): Unit = {
        generateeRewards = generateeRewards.filter{case(id, rewards) => id != articleId}
        articleRewards = articleRewards.map{case(fieldId, articleReward) => (fieldId, articleReward.filter{case(id, _) => id != articleId} )}.toMap
    }

    def removeHeadlineReward(articleId: ArticleID, generateeId: ModelID): Unit = {
        generateeRewards = generateeRewards.map{
            case(id, map) if id == articleId => (id, map.filter{case(id2, rewards) => generateeId != id2})
            case(x) => x
        }
    }
    //ADDING DROPPING
    //adding headline through reinitialisation - mutation would require mutable Maps
    def addHeadline(articleId: ArticleID, generatee: Generatee, reward: BetaDistribution): Unit = {
        if(candidatesI.keys.map(id => id == articleId).max == true) {
            candidatesI = candidatesI.map{
                case(id, list) if id == articleId => {
                    addHeadlineReward(articleId, reward)
                    (id, list ++ List(generatee))
                }
                case(id, list) => (id, list)
            }.toMap
            reinitialise(nFieldsI, candidatesI)
        } else{
            throw new Exception(articleId + " cannot be found")
        }
    }

    def addArticle(articleId: ArticleID, candidate: List[Generatee], articleReward: BetaDistribution, headlineRewards: List[BetaDistribution]): Unit = {
        require(candidate.length == headlineRewards.length, "list of generatees and list of generatee rewards must be of the same length")
        candidatesI.keys.map{id => if (id == articleId) throw new Exception(articleId.toString + " already exists.")}
        candidatesI = candidatesI ++ Map(articleId -> candidate)
        addArticleReward(articleId, articleReward, headlineRewards)
        reinitialise(nFieldsI, candidatesI)
    }

    def dropArticle(articleId: ArticleID): Unit = {
        if(candidatesI.contains(articleId)){
            removeArticleReward(articleId)
            candidatesI =  candidatesI.filter{case(id, list) => id != articleId}.toMap
            reinitialise(nFieldsI, candidatesI)
        } else {
            throw new Exception(articleId + " cannot be found")
        }
    }

    def filterOnHeadline(articleId: ArticleID, generatee: Generatee)(implicit identityGeneratees: (Generatee, Generatee) => Boolean): Unit = {
        if(candidatesI.keys.map(id => id == articleId).max == true) {
            candidatesI = candidatesI.map{
                case(id, list) if id == articleId => {
                    val (targetId, list2) = filterAndCompare(list, generatee)
                    removeHeadlineReward(articleId, targetId)
                    (id, list2)
                }
                case(id, list) => (id, list)
            }.toMap
            reinitialise(nFieldsI, candidatesI)
        } else{
            throw new Exception(articleId + " cannot be found")
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

    def dropHeadline(articleId: ArticleID, generatee: Generatee)(implicit identityGeneratees: (Generatee, Generatee) => Boolean): Unit = {
        filterOnHeadline(articleId, generatee)
    }
    def dropAllButHeadline(articleId: ArticleID, generatee: Generatee)(implicit identityGeneratees: (Generatee, Generatee) => Boolean): Unit = {
        filterOnHeadline(articleId, generatee)((g1, g2) => identityGeneratees(g1, g2) == false)
    }

    //Resetting rewards
    def resetRewards(): Unit = {
        val tuple = buildRewards(nFieldsI, candidatesI)
        articleRewards = tuple._1; generateeRewards = tuple._2
    }
    
}


