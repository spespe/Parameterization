/**
  * Created by Pietro.Speri on 08/11/2017.
  */
import scala.io.Source
class Grep (fileName:String){
  def grep(pattern:String)={
    val matches = Source.fromFile(fileName).getLines.toList
    println("Matches found: "+matches.size)
    matches.foreach(x=>println(x.trim))
  }

  def checkFile(width:Int)={
    def checkLines(line:String): Unit ={
      if(line.length>width)
        println(fileName+": "+line.trim)
    }
    val src = Source.fromFile(fileName)
    for(line<-src.getLines) checkLines(line)
  }
}
