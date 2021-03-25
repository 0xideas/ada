package ada.interface

trait LTree[A]{
    def disaggregate: List[List[A]] = {

        def inner(tree: LTree[A], acc: List[List[A]]): List[List[A]] = {
            tree match {
                case LBranch(value, branches) => {
                    if(branches.length > 0) branches.flatMap(branch => inner(branch, acc.map(a => a ++ List(value))))
                    else acc.map(a => a ++ List(value))
                }
                case LLeaf(value) => acc.map(a => a ++ List(value))
            }
        }
        inner(this, List(List()))
    }
}

case class LLeaf[A](value: A) extends LTree[A]
case class LBranch[A](value: A, branches: List[LTree[A]]) extends LTree[A]





val tree = LBranch(4, List(LBranch(3, List(LLeaf(5), LBranch(1, List()), LBranch(9, List(LLeaf(7))))), LBranch(7, List(LLeaf(1)))))
