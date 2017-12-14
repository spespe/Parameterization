import java.util.Dictionary

import sun.reflect.generics.tree.Tree

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object EmptyTree extends Tree1[Nothing]{

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

def maximumElem(t:Tree[Int]):Int = t match {
  case Leaf(x) => x
  case Branch(x,y) => maximumElem(x) max maximumElem(y)
}

def maximumElem2(t:Tree[Int]):Int = foldTree(t)(x => x)(_ max _)

def depth(t:Tree[Int]):Int = t match {
  case Leaf(x) => 0
  case Branch(x,y) => 1 + (depth(x) max depth(y))
}

def depth2(t:Tree[Int]):Int = foldTree(t)(x=>0)((a,b)=> 1 + (a max b))

def mapTree[A,B](t:Tree[A])(f: A => B):Tree[B] = t match {
  case Leaf(x) => Leaf(f(x))
  case Branch(x,y) => Branch(mapTree(x)(f),mapTree(y)(f))
}

def mapTree2[A,B](t:Tree[A])(f: A=>B):Tree[B] = foldTree(t)(x=>Leaf(f(x)):Tree[B])(Branch(_,_))

def foldTree[A,B](t:Tree[A])(f:A=>B)(g: (B,B)=>B):B = t match {
  case Leaf(x) => f(x)
  case Branch(x,y) => g(foldTree(x)(f)(g),foldTree(y)(f)(g))
}

type Dictionary[A] = BinTree[(String, A)]
//Search
def search[A](key:String, dict:Dictionary[A]):Option[A] = dict match {
  case Leaf => None
  case Branch ((k,v), l, r) if (k == key) => Some(v)
  case Branch ((k,v), l, r) if (k > key) => search(key,l)
  case Branch ((k,v), l, r) if (k < key) => search(key,r)
}

