package ada.enhancements
import ada.interface.{Tree, Leaf, Branch, Twig, Stub}
import scala.collection.mutable.ListBuffer

class EncodeDecodeTree[A](implicit f: A => String, g: String => A){
    //inefficient
    def splitIfNoBraces(string: String): Array[String] = {
        var braces = 0 
        var res = ListBuffer[ListBuffer[Char]](ListBuffer[Char]())
        string.map{c => {
            if("([{".contains(c)) braces += 1
            else if(")]}".contains(c)) braces -= 1
            c match {
                case ',' if braces == 0 => res.append(ListBuffer[Char]())
                case char => res.last.append(char)
            }
            require(braces >= 0, "Closing brace without matching opening brace found")
        }}
        res.map(_.mkString).toArray
    }
    def convertTreeToString(tree: Tree[A]): String = {
        tree match {
        case(Leaf(value)) => "(" + f(value) + ")"
        case(Twig(value, branch)) => "[" + f(value) + "," + convertTreeToString(branch) +  "]"
        case(Branch(branches)) => "{" + branches.map(branch => convertTreeToString(branch)).mkString(",") + "}"
        case(Stub()) => "|"
        }
    }
    def convertStringToTree(string: String): Tree[A] = {
        string.length match {
            case 0 => Stub()
            case _ => {
                string(0).toString match {
                    case("(") => Leaf(g(string.slice(1, string.length-1)))
                    case("[") => {
                        val split = splitIfNoBraces(string.slice(1, string.length-1))
                        if( split.length != 2 ) throw new Exception("Twig cannot be reconstructed from " + string)
                        val value = split(0)
                        val branch = split(1)
                        Twig(g(value), convertStringToTree(branch))
                    }
                    case("{") => {
                        val split = splitIfNoBraces(string.slice(1, string.length -1))
                        Branch(split.map(str => convertStringToTree(str)).toList)
                    } 
                    case("|") => Stub()
                }
            }
        }
    }
}