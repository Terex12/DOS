package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class Key extends Actor{
  println("AES Initialize ...")
  var cache = new HashMap[String, (String, String)]()
  println("HashMap : " + cache.isEmpty)
  
  def receive = {
    case StoreKey(id, key, iv) =>
      cache += (id -> (key, iv))
      println("size :  " + cache.size)
      cache.foreach(_ => println("In AES KEY id : " + id  + " key: " + key + " iv : " + iv)) 
      
    case getKey(id) =>
      val node = cache.get(id)
      node match{
        case Some(s) => sender ! s
        case _ => 
      }
  }
}