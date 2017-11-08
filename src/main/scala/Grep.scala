/**
  * Created by Pietro.Speri on 08/11/2017.
  */
import scala.io.Source
class Grep (fileName:String){
  def grep(pattern:String)={
    val matches = Source.fromFile("").getLines.toList
    println("Matches found: "+matches.size)
    matches.foreach(x=>println(x.trim))
  }
}
