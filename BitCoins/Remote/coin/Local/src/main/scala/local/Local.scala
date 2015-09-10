package local

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.routing.RoundRobinRouter
import java.security.{SecureRandom, MessageDigest}
import scala.collection.mutable.Map
import common.{MiningRequest, Work, RightResult, Coins, Available, Terminal, Stop, helptool}


object Local {

  class LocalActor(IP: String) extends Actor {
    var remoteClose = 0
    var numberOfSlave = 0
    val system = ActorSystem("LocalSystem")
    val remote = context.actorFor("akka.tcp://RemoteSystem@" + IP + "/user/RemoteActor")
    
    def receive = {
      case msg: String =>
        println(s"LocalActor received message: '$msg'")
        remote ! Available
      case Work(k: Int, prefix: String, times: Int) => {
        val processes = Runtime.getRuntime().availableProcessors()
        numberOfSlave = processes
        val workerRouter = context.actorOf(Props(new Slave(IP)).withRouter(RoundRobinRouter(numberOfSlave)), name = "workerRouter")        
        for (i <- 1 to numberOfSlave){
          workerRouter ! Work(k, prefix, times)
        }
      }
      case Terminal => {
        remoteClose += 1
        println("finish work ")
        Thread.sleep(10000)
        if (remoteClose == numberOfSlave)
          remote ! Terminal
          
      }
      case Stop => {
        println("Wait for while to shut down")
        Thread.sleep(8000)
        //context.system.shutdown()

      }
      case _ => 
        {println("Huh?")}
    }  
  }
  
  class Slave(IP: String) extends Actor {
    val remote = context.actorFor("akka.tcp://RemoteSystem@" + IP + "/user/RemoteActor")
    //val remote = context.actorFor("akka.tcp://RemoteSystem@127.0.0.1:2552/user/RemoteActor")
    var number = 0
    def receive = {
      case Work(k: Int, prefix: String, times: Int) => {
        val helper = new helptool()
        for (i <- 1 to times){
          val random = scala.util.Random.alphanumeric.take(8).mkString
          val hashString = helper.sha256(prefix, random)  //perform work, calculate hash
          if (helper.judgeCoins(k, hashString)){
            println("send right answer back")            //send result back
            number += 1
            remote ! RightResult(prefix+";"+random, hashString) //send result back
          }
        }
        println("remote get : " + number)
        Thread.sleep(number*200)    //wait finish transport
        sender ! Terminal
      }   
    }
  }

def main(args: Array[String]) {
    val system = ActorSystem("LocalSystem")
    //sbt run
    println("Enter 'k' - the number of leading zeroes : ")
    val k = readInt         //requirement
    println("Enter remote IP address : ")
    val IP = Console.readLine
    
    //scala LocalMinning.scala
    //val k = args(0).toInt
    val prefix = "xyfsoham"      //gatorID
    
    val localActor = system.actorOf(Props(new LocalActor(IP)), name = "LocalActor")
    localActor ! "The Slave is alive!"
  }
  
}