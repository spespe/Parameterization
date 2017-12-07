import sun.reflect.generics.tree.Tree

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object EmptyTree extends Tree[Nothing]{

  override def elem: Nothing = throw new NoSuchElementException

  override def left: Tree[Nothing] =  throw new NoSuchElementException

  override def right: Tree[Nothing] = throw new NoSuchElementException

}

trait Tree1[+T]{
  def elem: T
  def left:Tree[T]
  def right:Tree[T]
}

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

//Recursive implementation of size method
def size[A](t:Tree[A]):Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}
