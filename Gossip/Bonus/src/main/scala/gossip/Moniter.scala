import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class start(rounds: Int, nodeID: Int)  //start a moniter

class Moniter(numNode: Int, system: ActorSystem) extends Actor {
  var currentNodeNum: Int = numNode
  var getOnceNode: Int = 0
  var aliveNode: Int = 0
  var roundCount: Int = 0
  var starttime: Long = 0

  val duration1 = Duration(3, "s")
  val duration2 = Duration(3, "s")
  val duration3 = Duration(5, "s")
  val duration4 = Duration(3, "s")
  system.scheduler.schedule(duration1, duration2, self, "checkHealth")
  system.scheduler.schedule(duration3, duration4, self, "checkAlive")
  
  def receive = {
    case start(rounds, nodeID) =>{
      if (getOnceNode == 0) {
        starttime = System.currentTimeMillis()
      }
      println("Node " + nodeID + " got the message")
      getOnceNode += 1
      //println("Number of Node : " + numNode)
      //println("Number of Got message : " + getOnceNode)
      roundCount = rounds

      // if (getOnceNode+(numNode-currentNodeNum) >= numNode) {
      //   println("Round counter : " + roundCount)
      //   terminate
      // }
    }
  

  case "checkHealth" =>
      aliveNode = 0
      for (i <- 0 to numNode - 1) {
        var node = context.actorSelection("akka://GossipSystem/user/" + i.toString)
        node ! "Are you alive?"
      }

  case "I am alive" =>
      aliveNode += 1

  case "checkAlive" =>
      currentNodeNum = checkAliveNodes
      println("Num of Node get Once Info : " + getOnceNode)
      println("Alive Node : " + currentNodeNum)
      if (getOnceNode+(numNode-currentNodeNum) >= numNode) {
        println("Round counter : " + roundCount)
        terminate
      }
    }



  def checkAliveNodes = {
    println("Current aliveNodes : " + aliveNode)
    aliveNode
  }

  
  def terminate = {
    println("======================================================================================")
    println(" Time : " + (System.currentTimeMillis - starttime).toString + " ms")
    println("======================================================================================")
    system.shutdown
  }
}