import akka.actor.ActorSystem
import akka.actor.Props
import util.Random
import scala.collection.immutable.Map

object Gossip {
  def main(args: Array[String]) {
    if (args.length == 3){
      var numNode = args(0).toInt
      var topology = args(1)
      var algo = args(2)
      var system = ActorSystem("GossipSystem")
      val moniter = system.actorOf(Props(new Moniter(numNode, system)), name = "moniter")
      
      if (algo.equalsIgnoreCase("gossip")) {
        println("NetWork Initializing... \n")
        for (i <- 0 to numNode - 1) {
          var node = system.actorOf(Props(new Node(numNode, topology, algo, i, moniter)), name = i.toString)
        }
        if (topology == "imp3d"){
          var numNode1: Int = numNode
          if(numNode % 2 == 1)
            numNode1 = numNode-1
          val groups = Random.shuffle(List.range(0, numNode1)).splitAt(numNode1/2)
          println("Group1 : " + groups._1 + " || group2 : " + groups._2)
          val partnerMap = (groups._1 zip groups._2).toMap ++ (groups._2 zip groups._1).toMap
          println("Map : " + partnerMap)
          for (i <- 0 to numNode - 1) {
            var node = system.actorSelection("/user/" + i)
            node ! Map(partnerMap)
            node ! "Start"
          }
        }
        else{
          for (i <- 0 to numNode - 1) {
             var node = system.actorSelection("/user/" + i)
             node ! "Start"
          }
        }
        var firstNode = system.actorFor("/user/" + Random.nextInt(numNode))
        println("Start Node is : " + firstNode.path + '\n')
        println("Starting Gossip Algorothm... \n")
        moniter ! "Start Spread"
        firstNode ! gossipM("This is the message")
      }
      else {
        println("NetWork Initializing... \n")
         for (i <- 0 to numNode - 1) {
          var node = system.actorOf(Props(new Node(numNode, topology, algo, i, moniter)), name = i.toString)
        }
        if (topology == "imp3d"){
          var numNode1: Int = numNode
          if(numNode % 2 == 1)
            numNode1 = numNode-1
          val groups = Random.shuffle(List.range(0, numNode1)).splitAt(numNode1/2)
          println("Group1 : " + groups._1 + " || group2 : " + groups._2)
          val partnerMap = (groups._1 zip groups._2).toMap ++ (groups._2 zip groups._1).toMap
          println("Map : " + partnerMap)
          for (i <- 0 to numNode - 1) {
            var node = system.actorSelection("/user/" + i)
            node ! Map(partnerMap)
            node ! Set_s(i)
            node ! "Start"
          }
        }
        else{
          for (i <- 0 to numNode - 1) {
             var node = system.actorSelection("/user/" + i)
             node ! Set_s(i)
             node ! "Start"
          }
        }
        var firstNode = system.actorFor("/user/" + Random.nextInt(numNode))
        println("Start Node is : " + firstNode.path + '\n')
        println("Starting Push-Sum Algorithm... \n")
        moniter ! "Start Spread"
        firstNode ! pushSum(0.0, 0.0)
      }
    }
    else
      println("Input three arguments: <#nodes> <topology> <algorithm>")
  }
}