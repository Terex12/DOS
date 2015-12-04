package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet


class MMU extends Actor{
  println("Memory Initialize ...")
  var memory = new HashMap[String, Node]()
  println("HashMap : " + memory.isEmpty)
  
  var id  = -1
  
  def receive = {
    case AddEdge(nodeId: String, edgeName: String, id: String) => 
      val future = memory.get(nodeId)
      future match{
        case Some(node) =>node.getEdges.addEdge(edgeName, id)
        case _ =>
      }
      
      
    case Get(nodeId: String) => {
      val node=memory.get(nodeId)
      node match{
        case Some(s) => sender ! s
        case _ => 
      }
      
    } 
    case AddNode(fields: Fields, edges: Edges) =>
      id += 1
      fields.addID(id.toString())
      memory += (id.toString() -> new Node(fields, edges))
      sender ! id
      
    case Update(nodeId: String, para: Seq[(String,String)]) => {
      val node = memory.get(nodeId).get
      val array = node.getFields.getAll()
      val set = new HashSet[String]()
      array.foreach ({ case kv => set.add(kv.getKey()) })
      for (i<- 0 to para.size-1){
        if (set.contains(para(i)._1))
          node.getFields.update(para(i)._1, para(i)._2)
      }
    }
    
    case CreateFriendlist => {
      val userAmount = id
      //println("ENter CreateFriends .. user Amoutn : " + userAmount )
      for (i <- 1 to id){
        for (j <- 1 to 10){
          val fid = Random.nextInt(userAmount)
          val node = memory.get(i.toString()).get
          node.getEdges.addEdge("friendlist", fid.toString())
          val friend = memory.get(fid.toString()).get
          friend.getEdges.addEdge("friendlist", i.toString())
        }
      }
      sender ! "Finish"
    }
      
  }
}