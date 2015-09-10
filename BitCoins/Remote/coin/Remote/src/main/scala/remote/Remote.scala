package remote

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.routing.RoundRobinRouter
import java.security.{SecureRandom, MessageDigest}
import scala.collection.mutable.Map
import common.{MiningRequest, Work, RightResult, Coins, Available, Terminal, Stop, helptool}

/**
 * @author Terence
 */


object Remote {

  class RemoteActor(k: Int, prefix: String, listener: ActorRef) extends Actor{ 
    var remoteClose = 0
    var numberOfSlave = 0
    var remoteNumber = 0
    val times = 100
    val map = Map[String, String]()  //return result
  
    def receive = {
      case msg: String =>  
        println(s"RemoteActor received message: '$msg'")
        
      case Available => {
        remoteNumber += 1
        sender ! Work(k, prefix, times)
      }
      
      case MiningRequest => {
        val processes = Runtime.getRuntime().availableProcessors()
        numberOfSlave = processes
        val workerRouter = context.actorOf(Props[Slave].withRouter(RoundRobinRouter(numberOfSlave)), name = "workerRouter")
        for (i <- 1 to numberOfSlave){
          workerRouter ! Work(k, prefix, times)
        }
      }

      case RightResult(in, hashString) => 
        map += (in -> hashString)
  
      case Terminal => {
        println("Get terminal signal")
        println("Current connection number" + (numberOfSlave+remoteNumber))
        remoteClose += 1
        Thread.sleep(10000)
        sender ! Stop
        Thread.sleep(10000)
        if (remoteClose == numberOfSlave+remoteNumber)
          listener ! Coins(map)
      }
      case _ => {println("Huh?")}
    }
  }

  class Slave extends Actor {
    var number = 0
    def receive = {
      case Work(k: Int, prefix: String, times: Int) => {
        val helper = new helptool()
        for (i <- 1 to times){
          val random = scala.util.Random.alphanumeric.take(8).mkString
          val hashString = helper.sha256(prefix, random)  //perform work, calculate hash
          if (helper.judgeCoins(k, hashString)){
            number += 1
            sender! RightResult(prefix+";"+random, hashString) //send result back
          }
        }
        println("local get : " + number)
        Thread.sleep(number*500)    //wait finish transport
        sender ! Terminal
      }
      case Stop =>
        context.stop(self)
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
  
  
   def main(args: Array[String]) {
    val system = ActorSystem("RemoteSystem")
    //sbt run
    val k = Integer.parseInt(args(0))
    println("K is : " + k)
    //println("Enter 'k' - the number of leading zeroes : ")
    //val k = readInt         //requirement
    
    val prefix = "xyfsoham"      //gatorID
    
    val listener: ActorRef = system.actorOf(Props[Listener], name = "listener")
    val remoteActor: ActorRef = system.actorOf(Props(new RemoteActor(k,prefix,listener)), name = "RemoteActor")
    remoteActor ! "Remote System Starts"
    println("Waiting remote node connecting...")
    Thread.sleep(30000)
    println("No remote node connect, start local slave")
    remoteActor ! MiningRequest
  }

}
