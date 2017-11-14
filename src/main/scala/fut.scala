import java.io.{BufferedWriter, FileWriter}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object fut {

  def writeFile(fileName:String, text:String, mode:String): Unit ={
    val bw = new BufferedWriter(new FileWriter(fileName))
    mode match {
      case "a" => bw.append(text); bw.flush
      case "w" => bw.write(text); bw.flush
      case _ => {System.err.println("Error, the mode can be 'a' for append or 'o' for overwrite."); System.exit(1)}
    }
    bw.close
  }
}
