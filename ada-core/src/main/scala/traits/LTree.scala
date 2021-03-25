package ada.interface

trait LTree[A]

case class LBranch[A](val value: A, branch: LTree[A]) extends LTree[A]
case class LLeaf[A](val value: A) extends LTree[A]
case class LBough[A](branches: List[LTree[A]]) extends LTree[A]
case class LStub[A]() extends LTree[A]


//val tree = LBranch(4, List(LBranch(3, List(LLeaf(5), LBranch(1, List()), LBranch(9, List(LLeaf(7))))), LBranch(7, List(LLeaf(1)))))