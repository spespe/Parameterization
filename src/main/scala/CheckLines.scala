import scala.io.Source
/**
  * Created by Pietro.Speri on 07/11/2017.
  */

object CheckLines{
  def checkFile(fileName:String, width:Int){
    def checkLine(line:String){
      if(line.length>width){
        println(fileName+": "+line.trim)
      }
    }
    val source = Source.fromFile(fileName)
    for(line<-source.getLines)
      checkLine(line)
  }

  def isContainingAllUnique(s:String) = s match {
    case "" => false
    case s: String => if (s.length == s.distinct.length) true else false
    case _ => {
      println("[NOT A VALID STRING PASSED]")
      System.exit(1)
    }
  }

  //Recursive implementation of foldRight to be used for other methods
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }


  //Tail recursive implementation of foldLeft
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ds.length) -1
      else if (p(ds(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }

  def setHead(l: List[Any], v: Any): List[Any] = l match {
    case List() => sys.error("This is an empty list")
    case x :: xs => v :: xs
  }

  def check4Permutation(s1:String, s2:String):Boolean={
    if(s1.sorted.length==s2.sorted.length) true else false
  }

  def longestWord(fileName:String)={
    val word = Source.fromFile(fileName).getLines.flatMap(_.split(" ")).reduceLeft((a,b)=>if(a.length>b.length) a else b)
    println(word+": LENGTH["+word.length+"]")
  }

  def parseToArrayDouble(input:String):Array[Double]={
    val splitted = input.split(",")
    splitted.map(x=>x.toDouble)
  }

  def parseToTuple(input:String):(Int, Int, Double, Int)={
    val splitted = input.split(",")
    require(splitted.size==4)
    (splitted(0).toInt, splitted(1).toInt, splitted(2).toDouble, splitted(3).toInt)
  }
}

