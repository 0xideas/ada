package ada.interface

trait Tree[A]

case class Twig[A](val value: A, branch: Tree[A]) extends Tree[A]
case class Leaf[A](val value: A) extends Tree[A]
case class Branch[A](branches: List[Tree[A]]) extends Tree[A]
case class Stub[A]() extends Tree[A]


//val tree = LBranch(4, List(LBranch(3, List(LLeaf(5), LBranch(1, List()), LBranch(9, List(LLeaf(7))))), LBranch(7, List(LLeaf(1)))))