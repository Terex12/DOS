import akka.actor.ActorSystem
import akka.actor.Props
import util.Random
import scala.concurrent.duration._
import util.Random
import akka.actor.PoisonPill
import scala.concurrent.ExecutionContext.Implicits.global

object Gossip {
  
  var system: ActorSystem = null
  var numNode: Int = 0
  def main(args: Array[String]) {
    if (args.length == 3){
      numNode = args(0).toInt
      var topology = args(1)
      var algo = args(2)
      system = ActorSystem("GossipSystem")
      
      
      val moniter = system.actorOf(Props(new Moniter(numNode, system)), name = "moniter")
      
      
      if (algo.equalsIgnoreCase("gossip")) {
        println("Initializing... \n")
        for (i <- 0 to numNode - 1) {
          var node = system.actorOf(Props(new GossipNode(numNode, topology, algo, i, moniter)), name = i.toString)
          node ! "Start"
        }
        var firstNode = system.actorFor("/user/" + Random.nextInt(numNode))
        println("first node :: " + firstNode.path + '\n')

        println("Starting Gossip Algorothm... \n")
        firstNode ! loopGossip("This is the message")
      }
      else {
        println("Initializing... \n")
        for (i <- 0 to numNode - 1) {
          var node = system.actorOf(Props(new PushSumNode(numNode, topology, algo, i, moniter)), name = i.toString)
          node ! "Start"
        }
        var firstNode = system.actorFor("/user/" + Random.nextInt(numNode))
        println("Starting Push-Sum Algorithm... \n")
        firstNode ! loopPushSum(0.0, 0.0)
      }

      //wait 3 seconds then start kill node, every 3s, kill one
      val duration1 = Duration(3, "s")
      val duration2 = Duration(3, "s")
      system.scheduler.schedule(duration1, duration2)(killNode)
    }
    else
      println("Input three arguments: <#nodes> <topology> <algorithm>")
  }
   def killNode = {
      println("Start to kill node")
      var rand = Random.nextInt(numNode)
      var targetNode = system.actorFor("akka://GossipSystem/user/" + rand)
      println("Killing Node : " + targetNode.path)
      targetNode! PoisonPill
    }
}