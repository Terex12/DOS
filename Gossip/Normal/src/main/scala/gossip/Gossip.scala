import akka.actor.ActorSystem
import akka.actor.Props
import util.Random

object Gossip {
  def main(args: Array[String]) {
    if (args.length == 3){
      var numNode = args(0).toInt
      var topology = args(1)
      var algo = args(2)
      var system = ActorSystem("GossipSystem")
      
      
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
    }
    else
      println("Input three arguments: <#nodes> <topology> <algorithm>")
  }
}