import java.io.{BufferedWriter, FileNotFoundException, FileWriter, IOException}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object fut {

  def writeFile(fileName:String, text:String, overwrite:Boolean): Unit ={
    val bw = new BufferedWriter(new FileWriter(fileName, overwrite))
    try{
      overwrite match {
        case false => bw.append(text)
        case true => bw.write(text)
      }
      bw.flush
      bw.close
    } catch {
      case ex:FileNotFoundException => {
        println("[FILE NOT FOUND EXCEPTION]")
        println(ex.getMessage)
        println(ex.toString)
        ex.printStackTrace
      }
      case ex:IOException => {
        println("[IO EXCEPTION]")
        println(ex.getMessage)
        println(ex.toString)
        ex.printStackTrace
      }
      case _ =>
    }
  }

}

