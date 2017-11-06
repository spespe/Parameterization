import com.typesafe.scalalogging.slf4j.LazyLogging

/**
  * Created by Pietro.Speri on 17/10/2017.
  */

object type_parameterization extends LazyLogging {

  def main(args: Array[String]): Unit ={
    logger.info("[TYPE PARAMETERIZATION]")
    val c1 = new Cell[String]("ABC")
    val c2:Cell[String] = c1
    val s:String=c1.get
    List(c1,c2,s).foreach(println)

  }

  logger.info("[LOWER BOUNDS]")
  class Queue[+T] ( private val leading: List[T], private val trailing: List[T] ) {
    def enqueue[U >: T](x: U) =
      new Queue[U](leading, x :: trailing) // Type U is a superclass of the type T and is returning a type U
  }

  class Cell[T](init:T){
    override def toString: String = ""+init
    private[this] var current = init
    def get = current
    def set(x:T)= {current = x}
  }

}

trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}

object Queue {
  def apply[T](xs: T*): Queue[T] =
    new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[T]( private val leading: List[T], private val trailing: List[T]) extends Queue[T] {
    def mirror =
      if (leading.isEmpty)
        new QueueImpl(trailing.reverse, Nil)
      else
        this
    def head: T = mirror.leading.head
    def tail: QueueImpl[T] = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }
    def enqueue(x: T) =
      new QueueImpl(leading, x :: trailing)
  }
}

object extractor{
  def apply(user: String, domain:String): String = user + "@" + domain

  def unapply(str:String):Option[(String,String)]= {
    val parts=str.split("@")
    if (parts.length==2) Some(parts(0), parts(1)) else None
  }
}


