package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import spray.routing.{Route, HttpService, RequestContext}


case class MyGetRequest(path: List[String], para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef)
case class GetRequest(path: List[String], para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef)

case class PostRequest(path: List[String], para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef)
case class CreateUser(para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef)


case class AddEdge(nodeId:String, edgeName:String, id:String)

case class Get(nodeId: String)
case class AddNode(fields:Fields, edges:Edges)

case class Update(nodeId: String, para: Seq[(String,String)])

case class RegisterDone(mmu: ActorRef, ctx: RequestContext)
case object CreateFriendlist

case class Find(path: List[String], para: Seq[(String,String)])
