import scala.collection.mutable.ArrayBuffer
/**
  * Created by Pietro.Speri on 02/11/2017.
  */
object Queues {

  abstract class IntQueue {
    def get(): Int

    def put(x: Int)
  }

  class BasicIntQueue extends IntQueue {
    val buf = new ArrayBuffer[Int]

    def get(): Int = buf.remove(0)

    def put(x: Int) = {
      buf += x
    }
  }

  trait Incrementing extends IntQueue {
    abstract override def put(x: Int) = {
      super.put(x + 1)
    }
  }

  trait Doubling extends IntQueue {
    abstract override def put(x: Int) = {
      super.put(x * 2)
    }
  }

  trait Filtering extends IntQueue {
    abstract override def put(x: Int) = {
      if (x >= 0) super.put(x)
    }
  }

  case class LQueue(o:Stream[Int], oL:Int, i:List[Int], iL:Int){
    //push
    def pushing(p:Int){
      val q = queueMaker(o,oL,p::i,iL+1)
      println(s"pushing: ${p} - ${q}")
      q
    }
    //empty
    def empty = i.isEmpty && o.isEmpty
    //pop
    def pop:(Int,LQueue)={
      val s = (o.head, queueMaker(o.tail, oL-1, i, iL))
      println(s"pop: ${s._1} and ${s._2}")
      s
    }
  }

  //queueMake
  def queueMaker(o:Stream[Int], oL:Int, i:List[Int], iL:Int):LQueue = {
    if(iL <= oL) LQueue(o,oL,i,iL)
    else
      {
        //Using copy method here
        val iSize = 0
        val oSize = oL + iL
        val oStream = copy(o,i,Stream.empty)
        val iList = List()
        LQueue(oStream, oSize,iList, iSize)
      }
  }

  //copy
  def copy(o:Stream[Int], i:List[Int], iR:Stream[Int]):Stream[Int] = i match {
    case Nil => Stream.empty
    case p::pt if o.isEmpty => Stream.cons(p,iR)
    case p::pt => Stream.cons(o.head, copy(o.tail,i.tail,Stream.cons(p,iR)))
  }



}

