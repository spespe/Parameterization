import java.io.File

/**
  * Created by Pietro.Speri on 06/11/2017.
  */
object fileMatcher {
  def filesHere = (new File(".")).listFiles

  def fileMatching(matches : String =>Boolean): Unit ={
    for(file<-filesHere; if matches(file.getName)) yield file
  }

  def fileEnding(pattern:String)=fileMatching(_.endsWith(pattern))

  def fileContains(pattern:String)=fileMatching(_.contains(pattern))

  def fileRegex(pattern:String)=fileMatching(_.matches(pattern))
}
