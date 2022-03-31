package ada.interface

trait Tree[A]

case class Twig[A](val value: A, tree: Tree[A]) extends Tree[A]
case class Leaf[A](val value: A) extends Tree[A]
case class Branch[A](trees: List[Tree[A]]) extends Tree[A]
case class Stub[A]() extends Tree[A]


//val tree = LBranch(4, List(LBranch(3, List(LLeaf(5), LBranch(1, List()), LBranch(9, List(LLeaf(7))))), LBranch(7, List(LLeaf(1)))))

object Tree{
    def reduceTree[A](tree: Tree[A], f: (A, A) => A): A = {
        tree match {
            case Leaf(v) => v
            case Twig(v, tree2) => f(v, reduceTree[A](tree2, f))
            case Branch(trees) => throw new Exception("Branch cannot be reduced") 
        }
    }
    def concatenateTree[A](tree: Tree[A]): List[A] = {
        tree match {
            case Leaf(v) => List(v)
            case Twig(v, tree2) => List(v) ++ concatenateTree[A](tree2)
            case Branch(trees) => throw new Exception("Branch cannot be concatenated") 
        }
    }
}
