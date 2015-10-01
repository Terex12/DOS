
import akka.actor.ActorSystem
import akka.actor.Actor
import scala.sys.Prop
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.PoisonPill
import scala.collection.mutable.ListBuffer
import scala.util.Random

case class finish(nodeID: Int)  //start a moniter

class Moniter(numNode: Int, system: ActorSystem) extends Actor {
  var deadNode = 0
  var roundCount: Int = 0
  var starttime: Long = 0
  var wakeup: Int = 0
  
  def receive = {
    case "Start Spread" => {
      starttime = System.currentTimeMillis()
    }

    case "Wake Up" => {
      wakeup += 1
    }
    case finish(nodeID: Int) => {
      deadNode += 1
      if (deadNode == wakeup && wakeup < numNode){
        println("No Alive Node, Network not Converged")
        terminate
      }
      else if(deadNode == numNode)
        terminate
    }
  }
  
  def terminate = {
    println("//////////////////////////////////////////////////////////////////")
    println(" Time : " + (System.currentTimeMillis - starttime).toString + " ms")
    system.shutdown
  }
}