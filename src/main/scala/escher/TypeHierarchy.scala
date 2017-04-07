package escher

/** Useless yet */
class TypeHierarchy() {
  import TypeHierarchy._

  private val typeTree = new RootNode()

}

/** Useless yet */
object TypeHierarchy {
  import collection.mutable

  sealed trait TypeTree {
    val children: mutable.Set[TypeNode] = mutable.Set[TypeNode]()

    def printTree(tab: Int = 2): Unit = {
      val tabS = " " * tab
      printTree(0, (d,s) => {
        print(tabS * d)
        print(s)
      })
    }

    def printTree(depth: Int, printer: (Int, String) => Unit): Unit = {
      this match {
        case _: RootNode =>
          printer(depth, "- Root\n")
        case tn: TypeNode =>
          printer(depth, s"- ${tn.ty}\n")
      }
      children.foreach(_.printTree(depth+1, printer))
    }
  }

  class RootNode() extends TypeTree

  class TypeNode(val ty: Type) extends TypeTree{
    val parents: mutable.Set[TypeTree] = mutable.Set[TypeTree]()

    def connectToParent(p: TypeTree): Unit ={
      p.children += this
      parents += p
    }

    def deleteConnectionToParent(p: TypeTree): Unit ={
      p.children -= this
      parents -= p
    }
  }

  /**
    * try to insert this typeNode into this TypeTree if this type is not already in the tree
    * @return whether the node has been inserted
    */
  def insertTypeNode(tree: TypeTree, typeNode: TypeNode): Boolean = {
    val ty = typeNode.ty
    var isDirectChild = true
    tree.children.foreach { child =>
      if (ty instanceOf child.ty) {
        if (child.ty instanceOf ty)
          return false
        else {
          // ty is proper child of child.ty
          insertTypeNode(child, typeNode)
          isDirectChild = false
        }
      } else {
        if (child.ty instanceOf ty) {
          child.deleteConnectionToParent(tree)
          child.connectToParent(typeNode)
        }
      }
    }
    if(isDirectChild)
      typeNode.connectToParent(tree)
    true
  }

}


