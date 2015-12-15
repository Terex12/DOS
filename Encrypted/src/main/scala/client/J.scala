package client

import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import org.json4s.ShortTypeHints

trait Amber

//case class BalticAmber(size: Int) extends Amber

case class MinedAmber(key: String, value: String) extends Amber

object Amber {
  private implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[MinedAmber])))
  def toJson(ambers: List[(String, String)]): String = writePretty(ambers)
  def toJson(amber: Amber): String = writePretty(amber)
}