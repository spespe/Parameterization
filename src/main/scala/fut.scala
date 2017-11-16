import java.io.{BufferedWriter, FileNotFoundException, FileWriter, IOException}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object fut {

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case List() => Nil
      case x :: xs => drop(xs, n - 1)
    }

  def printExceptionDetails(ex:Exception, text:String): Unit ={
    println("["+text.toUpperCase+"]")
    println(ex.getMessage)
    println(ex.toString)
    ex.printStackTrace
  }

  def writeFile(fileName:String, text:String, overwrite:Boolean): Unit ={
    try{
      val bw = new BufferedWriter(new FileWriter(fileName, overwrite))
      overwrite match {
        case false => bw.append(text)
        case true => bw.write(text)
      }
      bw.flush
      bw.close
    } catch {
      case ex:IOException => printExceptionDetails(ex,"IO EXCEPTION")
      case _ =>
    }
  }

  def readFile(fileName:String){
    try{
      Source.fromFile(fileName).getLines.foreach(println)
    } catch {
      case ex:FileNotFoundException => printExceptionDetails(ex, "FILE NOT FOUND EXCEPTION")
      case ex:IOException => printExceptionDetails(ex, "IO EXCEPTION")
    }
  }

}

