/**
  * Created by Pietro.Speri on 06/11/2017.
  */
object XMLInfo {
  def main(args:Array[String])={

  }

  abstract class Inf {
    val description:String
    val date:String
    val domain:String
    val seconds:Int
    val orders:String

    override def toString = description

    def toXML =
      <information>
        <description>{description}</description>
        <date>{date}</date>
        <domain>{domain}</domain>
        <seconds>{seconds}</seconds>
        <orders>{orders}</orders>
      </information>
  }




}
