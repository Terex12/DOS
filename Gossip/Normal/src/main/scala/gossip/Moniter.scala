import akka.actor.ActorSystem
import akka.actor.Actor
import scala.sys.Prop
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.PoisonPill
import scala.collection.mutable.ListBuffer
import scala.util.Random

case class start(rounds: Int, nodeID: Int)  //start a moniter

class Moniter(numNode: Int, system: ActorSystem) extends Actor {
  var aliveNode = 0
  var roundCount: Int = 0
  var starttime: Long = 0
  
  def receive = {
    case start(rounds, nodeID) =>{
      if (aliveNode == 0) {
        starttime = System.currentTimeMillis()
      }
      println("Node " + nodeID + " got the request")
      aliveNode += 1
      //println("Number of Alive Node : " + aliveNode)
      roundCount = rounds
      if (aliveNode >= numNode) {
        println("Round counter : " + roundCount)
        terminate
      }
    }
  }
  
  def terminate = {
    println("======================================================================================")
    println(" Time : " + (System.currentTimeMillis - starttime).toString + " ms")
    println("======================================================================================")
    system.shutdown
  }
}