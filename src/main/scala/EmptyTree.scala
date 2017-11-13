import sun.reflect.generics.tree.Tree

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object EmptyTree extends Tree[Nothing]{

  override def elem: Nothing = throw new NoSuchElementException

  override def left: Tree[Nothing] =  throw new NoSuchElementException

  override def rigth: Tree[Nothing] = throw new NoSuchElementException

}

trait Tree[+T]{
  def elem: T
  def left:Tree[T]
  def rigth:Tree[T]
}

class Branch[+T](val elem:T, val left:Tree[T], val right:Tree[T]) extends Tree[T] {
  override def equals(other: Any):Boolean = other match {
    case that: Branch[_] => (that canEqual this ) && this.elem == that.elem && this.left == that.left && this.right == that.right
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Branch[_]]

  override def hashCode(): Int = (elem, left, right).##
}

