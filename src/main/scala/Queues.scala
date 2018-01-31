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

  case class LQueue(o:Stream[Int], oL:Int, i:List[Int], iL:Int)

  //queueMake
  def queueMaker(o:Stream[Int], oL:Int, i:List[Int], iL:Int):LQueue = {
    if(iL <= oL) LQueue(o,oL,i,iL)
    else
      {
        //Implementation using third method
      }
  }



}

