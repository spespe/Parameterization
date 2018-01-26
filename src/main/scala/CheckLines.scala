import CheckLines.mul

import scala.collection.immutable.Stream.cons
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

  //Drop tail recursive
  @annotation.tailrec
  def drop[A](l:List[A], n:Int):List[A] =
    if(n<=0) l
    else l match {
      case List() => Nil
      case _ :: t => drop(t, n-1)
    }

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
    case ::(x,xs) => reverseList2(xs)++List(x)
    case Nil => Nil
  }

  @annotation.tailrec
  def reverseLists[A](l:List[A],acc:List[A]):List[A] = l match {
    case Nil => acc
    case ::(x,xs) => reverseLists(xs,x::acc)
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

  //Head method
  def head[B](l:List[B]):B= l match {
    case Nil => sys.error("Empty list passed")
    case ::(x,_) => x
  }

  //Tail Recursive methods
  @annotation.tailrec
  def startsWith[A](l:List[A],l2:List[A]):Boolean = (l,l2) match {
    case (_, Nil) => true
    case (x::xs,y::ys) if x == y => startsWith(xs,ys)
    case _ => false
  }

  //ApplyList
  def applyList[A](s:A*):List[A]= s match {
  case Nil => Nil
  case _ => ::(s.head, applyList(s.tail:_*))
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

  //increase
  def increase(l:List[Int]):List[Int] = l match {
    case List() => List(1)
    case ::(0,ls) => ::(1,ls)
    case ::(1,ls) => ::(0,increase(ls))
    case _ => sys.error("Not a valid number")
  }

  //add
  def addition(l1:List[Int], l2:List[Int]):List[Int] = (l1,l2) match {
    case (Nil,Nil) => Nil
    case (Nil,x) => x
    case (x, Nil) => x
    case (x::xp,0::yp) => ::(x,addition(xp,yp))
    case (0::xp,y::yp) => ::(y,addition(xp,yp))
    case (x::xp,0::yp) => ::(0,addition(xp,yp))
    case _ => sys.error("Not a valid number")
  }

  //take
  def take(i:Int,l:List[Int]):List[Int] = (i,l) match {
    case (0,p) => p
    case (1,Nil) => List(1)
    case (1,p::ps) => (1-p)::take(p,ps)
    case (_,_) => throw new IllegalArgumentException("The input passed it is not valid.")
  }

  //add method
  def add(a:Int, l1:List[Int], l2:List[Int]):List[Int] = (l1,l2) match {
    case (List(),List()) => take(a, List())
    case (List(), _ :: _) => take(a,l2)
    case (_ :: _, List()) => take(a,l1)
    case (p::ps,q::qs) => ((a+p+q)%2)::add((a+p+q)/2,ps,qs)
  }

  //xtimes
  def xtimes(l1:List[Int], l2:List[Int]):List[Int] = {
    def mul(p:List[Int], q:List[Int]):List[Int] = (p) match {
      case Nil => Nil
      case ::(0,xs) => ::(0,mul(xs,q))
      case ::(1,xs) => add(0,q,::(0,mul(xs,q)))
    }
    val r = mul(l1.reverse, l2.reverse)
    r.reverse
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

  //Progression
  def progression(d1:Double, d2:Double):Stream[Double] = cons(d1,progression(d1+d2,d2))

  //Tail recursive gcd implementation
  def gcd(a:Int,b:Int):Int = if(b==0) 0 else gcd(b,a%b)

  def part[P <% Ordered[P]](el:P, s:List[P], fp:List[P], sp:List[P]):(List[P],List[P])= s match {
    case ::(xs,sl) => if(xs<el) part(el,sl,xs::fp,sp) else part(el,sl,sp,xs::sp)
    case List() => (fp,sp)
  }

  //getBigger
  def getBigger[P <% Ordered[P]](d:List[P]):(P,List[P]) = d match {
    case (Nil) => (null.asInstanceOf[P],List())
    case ::(f,Nil) => (f,List())
    case ::(f,v) => {
      val (p,s) = getBigger(v)
      if(p>=f) (p,::(f,s)) else (f,::(p,s))
    }
  }

  //sortBub
  def sortBub[P <% Ordered[P]](d:List[Int]):List[Int] = d match {
    case List() => Nil
    case _ => val (f,s) = getBigger(d)
      sortBub(s):::List(f)
  }

  //sortSel
  def sortSel[P <% Ordered[P]](d:List[P]):List[P] = d match {
    case Nil => Nil
    case ft::Nil => List(ft)
    case ft::fv => {
      val min = fv.min
      val minInd = fv.indexOf(min)
      if(ft <= min) ft::sortSel(fv) else {
        val (fp,sp) = fv.splitAt(minInd)
        sp.head::sortSel(fp:::ft::sp.tail)
      }
    }
  }

  //sortIns
  def sortIns[P <% Ordered[P]](el:P, d:List[P]):List[P]  = {
    if(d == Nil){
      el::d
    }
    val first::last = d
    if(first < el)
      first::sortIns(el,last)
    else
      el::d
  }

  //sortIns2
  def sortIns2[P <% Ordered[P]](d:List[P]):List[P] = {
    if (d == Nil)
      Nil
    else {
      val first = d.head
      val last = d.tail
      val t = sortIns2(last)
      sortIns(first, t)
    }
  }

  //splitSort
  def splitSort[P <% Ordered[P]](l:List[P]):(List[P],List[P]) = {
    if(l==Nil) return(Nil,Nil)
    val lh = l.head
    val lt = l.tail
    //Nil case
    if(lt==Nil) return(lh::Nil,Nil)
    val lth = lt.head
    val ltt = lt.tail
    val (tl1,tl2) = splitSort(lt.tail)
    (lh::tl1,lth::tl2)
  }

  //merge
  def merge[P <% Ordered[P]](l1:List[P],l2:List[P]):List[P] = (l1, l2) match {
    case (l1,Nil) => l1
    case (Nil,l2) => l2
    case (l1h::l1t, l2h::l2t) => if(l1h>l2h) l2h::merge(l1,l2t) else l1h::merge(l1t,l2)
  }

  //mergeS
  def mergeS[P <% Ordered[P]](l:List[P]):List[P] = {
    if(l==Nil||l.tail==Nil) l
      //using splitSort method
    val (lh,lt) = splitSort(l)
    val lms1 = mergeS(lh)
    val lms2 = mergeS(lt)
    //using merge method
    merge(lms1,lms2)
  }

  //pivotS
  def pivotS[P <% Ordered[P]](l:List[P]):List[P] = l match {
    case List() => List()
    case p::ps => {
      //using part method defined above
      val (l1,l2) = part(p,ps,List(),List())
      val ls1 = pivotS(l1)
      val ls2 = pivotS(l2)
      val tmp = p::ls2
      ls1 ++ tmp
    }
  }

}

