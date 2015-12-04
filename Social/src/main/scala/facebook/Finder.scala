package facebook

import scala.collection.mutable
import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import scala.concurrent._
import akka.util.Timeout
import scala.concurrent.duration._

class Finder(mmu: ActorRef) extends Actor{
  val duration = Duration(5, SECONDS)
  implicit val timeout = Timeout(duration)
  def receive = {
    case Find(path: List[String], para: Seq[(String,String)]) => {
      val respond = sender
      //println("I need to find %s and %s".format(path.toString(),para.toString()))
      var nodeId = ""
      if(((!para.isEmpty)&&(!para(0)._2.equals(""))&&(para(0)._2.matches("[0-9]*")))||(path.last.matches("[0-9]*"))){
        if((!para.isEmpty)&&(!para(0)._2.equals(""))&&(para(0)._2.matches("[0-9]*"))) 
          nodeId = (para(0)._2)
        else 
          nodeId = path.last

        //println("to find %s".format(nodeId))
        val futureNode:Future[Node] = (mmu ? Get(nodeId)).mapTo[Node]
        futureNode.onSuccess {
          case node: Node => {
            val fields = node.getFields
            val edges = node.getEdges
            if(para.isEmpty){
              respond ! new Object(fields.getAll().asInstanceOf[Array[Attribute]])
            }
            else {
              val Attributes: Array[KeyValue] = para.toArray.map(p => {
                val index = fields.find(p._1)
                if (index != (-1))
                  fields.get(index)
                else
                  new KeyValue(p._1, "404 Not Found")
              })
              //println("I GET RESULT")
              respond ! new Object(Attributes.asInstanceOf[Array[Attribute]])
            }
          }
        }
        futureNode.onFailure{
          case exc => {respond ! new Object(Array(new KeyValue(nodeId,"404 Not Found")))}
        }
      }
      else{
        var index = path.size-1
        while(!path(index).matches("[0-9]*"))
          index -= 1
        nodeId = path(index)
        val futureNode: Future[Node] = (mmu ? Get(nodeId)).mapTo[Node]
        futureNode.onSuccess {
          case node: Node => {
            val fields = node.getFields
            val edges = node.getEdges

            val listOfFuture: List[Future[Object]] = edges.getKeySet(edges.find(path(index+1))).getValue().toList.map(id => {
              var newPath = List(id)++path.drop(index+2)
              val subActor = context.system.actorOf(Props(classOf[Finder],mmu))
              (subActor ? Find(newPath, para)).mapTo[Object]
            })
            val futures: Future[List[Object]] = Future.sequence(listOfFuture).mapTo[List[Object]]
            futures.onSuccess{
              case result: List[Object] => respond! new Object(Array(new ArrayObject(path(index+1), result.toArray).asInstanceOf[Attribute]))
            }
            futures.onFailure{
              case exc => respond ! new Object(Array(new KeyValue("Failure","404").asInstanceOf[Attribute]))
            }

          }
        }
        futureNode.onFailure{
          case exc => {sender ! new Object(Array(new KeyValue("id", "404 Not Found")))}
        }
      }
    }
  }
}

