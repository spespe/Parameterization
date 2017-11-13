/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object emptyTree extends Tree[Nothing]{

  override def elem: Nothing = throw new NoSuchElementException

  override def left: Tree[Nothing] =  throw new NoSuchElementException

  override def rigth: Tree[Nothing] = throw new NoSuchElementException

}

trait Tree[+T]{
  def elem: T
  def left:Tree[T]
  def rigth:Tree[T]
}

