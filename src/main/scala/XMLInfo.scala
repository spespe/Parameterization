/**
  * Created by Pietro.Speri on 06/11/2017.
  */
object XMLInfo {
  def main(args:Array[String])={
    val inf = new Inf {
      val description = args(0).toString
      val date = args(1).toString
      val domain = args(2).toString
      val seconds = args(3).toInt
      val orders = args(4).toString
    }

    val node = inf.toXML
    scala.xml.XML.save(args(5).toString+"information.xml", node)

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


  def fromXML(node: scala.xml.Node) = new Inf {
    val description = (node \ "description").text
    val date = (node \ "date").text
    val domain = (node \ "domain").text
    val seconds = (node \ "seconds").text
    val orders = (node \ "orders").text
  }

}
