import scala.io.Source
import scala.util.Random
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


  //Append
  def append[T](xs:List[T], ys:List[T]):List[T] = xs match {
    case List() => ys
    case x::xs1 => x::append(xs1,ys)
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

  //Tail implementation
  def tail[A](l: List[A]): List[A] = l match {
    case List() => sys.error("This is an empty list")
    case x :: xs => xs
  }

  //Without using Random.shuffle
  def randomShuffle[A](xs:List[A]):List[A]={
    val seq = xs.permutations.toSeq
    seq.take(Random.nextInt(seq.size)+1).last
  }

  //Tail recursive implementation of foldLeft
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  //FoldRight via foldLeft
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverseList(l), z)((b,a) => f(a,b))

  //FoldLeft via foldRight
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  @annotation.tailrec
  def dropWhile[A](l1: List[A], f: A => Boolean): List[A] = l1 match {
    case x :: xs if (f(x)) => dropWhile(xs, f)
    case _ => l1
  }

  //Recursive implementation of dropWhile curried
  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case x :: xs if f(x) => x :: dropWhileCurried(xs)(f)
    case _ => l
  }

  //Recursive init method
  def init[A](l: List[A]): List[A] = l match {
    case List() => sys.error("The list passed is empty.")
    case _ :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

  def reverseList[A](l:List[A]):List[A] = foldLeft(l,List[A]())((acc,h)=>h::acc)

  def reverseList2[A](l:List[A]):List[A] = l match {
    case x::xs => reverseList2(xs)++List(x)
    case Nil => Nil
  }

  def orderWordsLength(l:List[String]) = l.groupBy(x=>x).map(x=>(x._1,x._2.length)).toList.sortBy(_._1).sortBy(_._2)

  def append2[A](l:List[A], l2:List[A]):List[A] = foldRight(l,l2)(_::_)

  def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ds.length) -1
      else if (p(ds(n))) n
      else loop(n + 1)
    }
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

  def getHead[Any](l:List[Any]):Any = l match {
    case List() => sys.error("This is an empty list")
    case x::xs => x
  }

  def check4Permutation(s1:String, s2:String):Boolean={
    if(s1.sorted.length==s2.sorted.length) true else false
  }

  def longestWord(fileName:String)={
    val word = Source.fromFile(fileName).getLines.flatMap(_.split(" ")).reduceLeft((a,b)=>if(a.length>b.length) a else b)
    println(word+": LENGTH["+word.length+"]")
  }

  def maximumElement(xs:List[Int]):Int = xs.reduce((a,b)=>if(a>b) a else b)
  
  //Generic
  def lengthList[A](l:List[A])=l.foldLeft(0)((a,_)=>a+1)

  def parseToArrayDouble(input:String):Array[Double]={
    val splitted = input.split(",")
    splitted.map(x=>x.toDouble)
  }

  def addString(l:List[String],s:String):List[String] = foldRight(l,Nil:List[String])((x,y)=>x+s::y)

  //Transformation via foldRight
  def transformDouble2String(l:List[Double]):List[String] = foldRight(l,Nil:List[String])((x,xs) => x.toString::xs)

  //Generic filter implementation via foldRight
  def filter[A](l:List[A])(p:A=>Boolean):List[A]= foldRight(l,Nil:List[A])((x,xs) => if(p(x)) x::xs else xs)

  //Reimplementing map method
  def map[A,B](l:List[A])(f:A=>B):List[B] = foldRight(l,Nil:List[B])((x,xs)=>f(x)::xs)

  //Creation of concat method using the foldRight
  def concat2[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  //Flatmap using concat2 method
  def flatMap[A,B](l:List[A])(f:A=>List[B]):List[B] = concat2(map(l)(f))

  //Filter via flatMap
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def parseToTuple(input:String):(Int, Int, Double, Int)={
    val splitted = input.split(",")
    require(splitted.size==4)
    (splitted(0).toInt, splitted(1).toInt, splitted(2).toDouble, splitted(3).toInt)
  }

  def addElem(l:List[Int], l2:List[Int]):List[Int] = (l,l2) match {
    case (Nil, _) => l2
    case (_, Nil) => l
    case (x::xs,y::ys) => x+y::addElem(xs,ys)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map(f(h), traverse(t)(f))(_ :: _)
    }

  def lenghtList[A](l: List[A]): Int = foldRight(l, 0)((_, x) => x + 1)

  //Generalization of addElem
  def addElemGeneric[A,B,C](l:List[A],l2:List[B])(f:(A,B)=>C):List[C] = (l,l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x::xs,y::ys) => f(x,y)::addElemGeneric(xs,ys)(f)
  }

  //Tail Recursive methods
  @annotation.tailrec
  def startsWith[A](l:List[A],l2:List[A]):Boolean = (l,l2) match {
    case (_, Nil) => true
    case (x::xs,y::ys) if x == y => startsWith(xs,ys)
    case _ => false
  }

  @annotation.tailrec
  def hasSubSequence[A](l:List[A],l2:List[A]):Boolean = l match {
    case Nil => l2 == Nil
    case _ if startsWith(l,l2) => true
    case x::xs => hasSubSequence(xs,l2)
  }

  //Faster than sum using fold
  def summing(l: List[Int]): Int = l match {
    case List() => 0
    case ::(x, xs) => x + summing(xs)
  }

  //Mean for sequence of doubles
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //Mean version2
  def mean(s:Seq[Double]):Double = s match {
    case Nil => throw new ArithmeticException("The sequence is empty!")
    case _ => s.sum/s.length
  }

  //Tail recursive gcd implementation
  def gcd(a:Int,b:Int):Int = if(b==0) 0 else gcd(b,a%b)

}

