package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import spray.routing.{Route, HttpService, RequestContext}
import rsa._


case class GetRequest(path: List[String], para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef, cache: ActorRef, key: ActorRef,rsa: RSA)
case class PostRequest(path: List[String], para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef, cache: ActorRef, key: ActorRef,rsa: RSA)
case class CreateUser(path: List[String], para: Seq[(String,String)], ctx: RequestContext, mmu: ActorRef, cache: ActorRef, key: ActorRef,rsa: RSA)
case class CreateEmptyUser(ctx: RequestContext, mmu: ActorRef, cache: ActorRef, key: ActorRef, para: Seq[(String,String)], rsa: RSA)

case class AddEdge(nodeId:String, edgeName:String, id:String)

case class Get(nodeId: String)
case class AddNode(fields:Fields, edges:Edges)

case class Update(nodeId: String, para: Seq[(String,String)])

case class RegisterDone(mmu: ActorRef, ctx: RequestContext)
case object CreateFriendlist

case class Find(path: List[String], para: Seq[(String,String)])

case class Store(id: String, access: String, key: BigInt, modu: BigInt)
case class StoreKey(id: String, key: String, iv:  String)

case class Permission(currentId:String, nodeId:String)

case class getId(ac: String)
case class getKey(id: String)
case class getAll(ac: String)
case class getAESKey(ac: String)

case object TestGet



