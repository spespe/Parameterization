import scala.io.Source
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

  def longestWord(fileName:String)={
    val word = Source.fromFile(fileName).getLines.flatMap(_.split(" ")).reduceLeft((a,b)=>if(a.length>b.length) a else b)
    println(word+": LENGTH["+word.length+"]")
  }

  def parseToArrayDouble(input:String):Array[Double]={
    val splitted = input.split(",")
    splitted.map(x=>x.toDouble)
  }

  def parseToTuple(input:String):(Int, Int, Double, Int)={
    val splitted = input.split(",")
    require(splitted.size==4)
    (splitted(0).toInt, splitted(1).toInt, splitted(2).toDouble, splitted(3).toInt)
  }
}

