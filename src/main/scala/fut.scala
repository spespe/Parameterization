import java.io.{BufferedWriter, FileWriter}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

/**
  * Created by Pietro.Speri on 13/11/2017.
  */
object fut {

  def writeFile(fileName:String, text:String, overwrite:Boolean): Unit ={
    val bw = new BufferedWriter(new FileWriter(fileName, overwrite))
    overwrite match {
      case false => bw.append(text)
      case true => bw.write(text)
    }
    bw.flush
    bw.close
  }

}

