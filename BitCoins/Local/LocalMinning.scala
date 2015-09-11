//package edu.ufl.xux

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.routing.RoundRobinRouter
import java.security.{SecureRandom, MessageDigest}
import scala.collection.mutable.Map


/**
 * @author Terence
 */


object LocalMinning {
  sealed trait Message
  case object MiningRequest extends Message  
  //case class Work(k: Int, prefix: String, random: String) extends Message
  case class Work(k: Int, prefix: String, times: Int) extends Message
  case class RightResult(in: String, hashString: String) extends Message 
  case class WrongResult(in: String, hashString: String) extends Message 
  case class Coins(coins: Map[String, String]) extends Message
  case object Terminal extends Message
  case object Available extends Message
  

  class Master(k: Int, prefix: String, listener: ActorRef) extends Actor{ 
    var remoteClose = 0
    var numberOfSlave = 0
    val map = Map[String, String]()  //return result
  
    def receive = {
      case MiningRequest => {
        val processes = Runtime.getRuntime().availableProcessors()
        numberOfSlave = processes*8
        //println("NUmber : " + numberOfSlave)
        val times = 1000000;
        val workerRouter = context.actorOf(Props[Slave].withRouter(RoundRobinRouter(numberOfSlave)), name = "workerRouter")
        for (i <- 1 to numberOfSlave){
          workerRouter ! Work(k, prefix, times)
        }
      }
      case RightResult(in, hashString) => map += (in -> hashString)
  
      case Terminal => {
        remoteClose += 1
        if (remoteClose == numberOfSlave)
          listener ! Coins(map)
      }
      case _ => {println("Huh?")}
    }
  }

  class Slave extends Actor {
    def receive = {
      case Work(k: Int, prefix: String, times: Int) => {
        val helper = new helptool()
        for (i <- 1 to times){
          val random = scala.util.Random.alphanumeric.take(8).mkString
          val hashString = helper.sha256(prefix, random)  //perform work, calculate hash
          if (helper.judgeCoins(k, hashString))
            sender! RightResult(prefix+";"+random, hashString) //send result back
        }
        sender ! Terminal
      }   
    }
  }

  class Listener extends Actor {
    def receive = {
      case Coins(map) => {
        if (map.size == 0)
          println("No Coins Get!!")
        else
          map.foreach{case(key, value) => println(key + " : " + value)}
        context.system.shutdown()
      }
    }
  }
  
  
  class helptool{
    //SHA256 hash function
    def sha256(prefix: String, random: String): String = {
      val md = MessageDigest.getInstance("SHA-256");
      val plaintext = prefix + ";" + random
      md.update(plaintext.getBytes("UTF-8")); // Change this to "UTF-16" if needed
      val digest = md.digest()
      val hexString = new StringBuffer();
      for ( j <- 0 to digest.length-1) {
        val hex = Integer.toHexString(0xff & digest(j))
        if(hex.length() == 1)  hexString.append('0')  //hex number must be
        hexString.append(hex);
      }
      hexString.toString()
    } 
    //check coins
    def judgeCoins(k: Int, hash: String) = {
      if (hash.startsWith("0" * k))
        true
      else
        false
    }
  }
  
   def main(args: Array[String]) {
    val system = ActorSystem("CoinMiningSystem")
    //sbt run
    //println("Enter 'k' - the required number of leading zeroes : ")
    //val k = readInt         //requirement
    val k = Integer.parseInt(args(0))
    println("K is : " + k)
    
    //scala LocalMinning.scala
    val prefix = "xyfsoham"      //gatorID
    
    val listener: ActorRef = system.actorOf(Props[Listener], name = "listener")
    val master: ActorRef = system.actorOf(Props(new Master(k,prefix,listener)))
    //val slaveActorRef:ActorRef = system.actorOf(Props[SlaveActor], name = "SlaveActor")    
    master ! MiningRequest
  }

}
