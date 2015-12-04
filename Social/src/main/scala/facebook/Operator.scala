package facebook

import akka.actor.{Props, Actor}
import org.json4s.DefaultFormats
import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import spray.routing.{Route, HttpService, RequestContext}
import akka.dispatch._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern.ask
import spray.http.MediaTypes._
import spray.http._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global

class Operator extends Actor{
  val duration = Duration(5, SECONDS)
  implicit val timeout = Timeout(duration)
  val json4sFormats = DefaultFormats
  
  def receive = {
    case MyGetRequest(path, para, ctx, mmu) =>{
      println("Enter MyGetRequest == Do Something Later...")
      println("Path : " + path)
      for (i<- 0 to para.size-1)
        println("(" + para(i)._1 + " : " + para(i)._2 +")")
      
      val finder: ActorRef = context.actorOf(Props(classOf[Finder], mmu))
      val future = (finder ? Find(path, para)).mapTo[Object]
      
      future.onSuccess{
        case result => {
          ctx.complete(result.toString() + "\n")
          this.context.stop(self)
        }
      }
      future.onFailure{
        case error => {
          ctx.complete("404 Not Found\n")
          this.context.stop(self)
        }
      }
    }
    
    case GetRequest(path, para, ctx, mmu) =>{
      println("Enter NormalGet == Do Something Later...")
      println("Path : " + path)
      for (i<- 0 to para.size-1)
        println("(" + para(i)._1 + " : " + para(i)._2 +")")
        
      val finder: ActorRef = context.actorOf(Props(classOf[Finder], mmu))
      val future = (finder ? Find(path, para)).mapTo[Object]
      future.onSuccess{
        case result => {
          ctx.complete(result.toString()+"\n")
          this.context.stop(self)
        }
      }
      future.onFailure{
        case error => {
          ctx.complete("Not Found\n")
          this.context.stop(self)
        }
      }

    }
    
    case PostRequest(path, para, ctx, mmu) =>{
      //interpret path
      var index = 0
      val p = path.reverse
      println("Post Request : " + p)
      if (p(0).matches("[0-9]*")){  //update exist node
        println("Enter IF")
        mmu ! Update(p(0), para)
        val res = HttpResponse(200)
        ctx.complete(res)
        this.context.stop(self)
      }
      else{                         //create new node         
        var edge: Edges = null
        var t = ""
        if (p(0).equals("posts")){
          edge = new Edges(Array())
          t = "post"
        }
        else if (p(0).equals("albums")){
          edge = new Edges(Array("photos"))
          t = "album"
        }
        else if (p(0).equals("photos")){
          edge = new Edges(Array())
          t = "photo"
        }
        val array = ListBuffer[KeyValue]()
        array += (new KeyValue("id", ""))
        array += (new KeyValue("type", t))
        for (i<- 0 to para.size-1){
          println("(" + para(i)._1 + " : " + para(i)._2 + ")")
          array += (new KeyValue(para(i)._1, para(i)._2))
        }
        val fields: Fields = new Fields(array.toArray)
        val future = mmu ? AddNode(fields, edge) 
        val result = Await.result(future, timeout.duration).asInstanceOf[Int]
        mmu ! AddEdge(p(1), p(0), result.toString())
        val entity = HttpEntity("Created " + result)
        val res = HttpResponse(201, entity = entity)
        ctx.complete(res)
        this.context.stop(self)
      }
      
    }
    
    case CreateUser(para, ctx, mmu) =>{
      println("In OP CreateUser ")
      val array = ListBuffer[KeyValue]()
      array += (new KeyValue("id", ""))
      array += (new KeyValue("type", "user"))
      for (i<- 0 to para.size-1){
        println("(" + para(i)._1 + " : " + para(i)._2 + ")")
        array += (new KeyValue(para(i)._1, para(i)._2))
      }
      val fields: Fields = new Fields(array.toArray)
      val edge: Edges = new Edges(Array("friendlist", "posts", "albums"))

      val future = mmu ? AddNode(fields, edge) 
      val result = Await.result(future, timeout.duration).asInstanceOf[Int]
      val entity = HttpEntity("Created " + result)
      val res = HttpResponse(201, entity = entity)
      ctx.complete(res)
      this.context.stop(self)
    }
    
    case RegisterDone(mmu, ctx) =>{
      println("************Register Done***************")
      val future = mmu ? CreateFriendlist
      val result = Await.result(future, timeout.duration).asInstanceOf[String]
      ctx.complete("FriendList Done")
    }
  }
}