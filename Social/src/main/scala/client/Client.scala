package client

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.util.Timeout
import spray.client.pipelining.sendReceive
import spray.httpx.RequestBuilding
import spray.http.HttpResponse
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.collection.mutable.ListBuffer
import util.Random

class SingleClient(behavior: String) extends Actor with RequestBuilding {
  val duration = Duration(60, SECONDS)
  implicit val requestTimeout = Timeout(duration)
  implicit val executionContext = context.dispatcher
  
  val clientPipeline = sendReceive
  var id = -1
  var album = -1
  
  def receive = {
    case Register(name, bod) => {
      //println("Enter Register =======")
      val url = "http://127.0.0.1:8080/page?name=" + name + "&bod="+ bod 
      println("URL : " + url)
      val fResponse: Future[HttpResponse] = clientPipeline {
        Post(url)
      }
      //in response package, it should encapsulate user id late(access token)
      fResponse onComplete {
        case Success(response) => {
          println(response.status.value)
          val s = response.entity.asString.split(" ")
          //println("*****************************" + s.toList.toString())
          id = s(s.size-1).trim().toInt
        }
        case Failure(error) => println(error.getMessage)
      }
      
      Thread.sleep(500)
    }
    
    case FinishAll(s) => {
      println("Enter Finishi ALL")
      val fResponse: Future[HttpResponse] = clientPipeline {
        Get("http://127.0.0.1:8080/registerdone")
      }
      fResponse onComplete {
        case Success(response) =>  
          println(response.status.value)
          s ! Done
        case Failure(error) => 
          println(error.getMessage)
          s ! Error("Not Finish Register")
      }
    }
    
    case StartPost =>{
      val url1 = "http://127.0.0.1:8080/" + id + "/albums?name=Santa&intro=UF"
      println("URL album is " + url1)
      val fResponse1 = clientPipeline {
        Post(url1)
      }
       fResponse1 onComplete {
        case Success(response) => {
          println(response.status.value)
          val s = response.entity.asString.split(" ")
          album = s(s.size-1).trim().toInt
        }
        case Failure(error) => println(error.getMessage)
      }
      Thread.sleep(200)
      var url = "http://127.0.0.1:8080/" + id + "/posts?title=Hello&content=World"
      var fResponse: Future[HttpResponse] = clientPipeline {
        Post(url)
      }
      fResponse onComplete {
        case Success(response) => println(response.status.value)
        case Failure(error) => println(error.getMessage) 
      }
      Thread.sleep(200)
      url = "http://127.0.0.1:8080/" + id + "/posts?title=Not&content=Good"
      fResponse = clientPipeline {
        Post(url)
      }
      fResponse onComplete {
        case Success(response) => println(response.status.value)
        case Failure(error) => println(error.getMessage) 
      }
      
      Thread.sleep(200)
      url = "http://127.0.0.1:8080/" + id + "/posts?title=OK&content=Fine"
      fResponse = clientPipeline {
        Post(url)
      }
      fResponse onComplete {
        case Success(response) => println(response.status.value)
        case Failure(error) => println(error.getMessage) 
      }       
      Thread.sleep(1000)
      sender ! FinishIniPost
    }
    
    case Simu =>{ 
      var postInterval= Duration(10, SECONDS)
      var getInterval= Duration(10, SECONDS)
      if (behavior.equals("Normal")){
       // postInterval
      }
      else if (behavior.equals("Crazy_Poster")){
        postInterval= Duration(5, SECONDS)
      }
      else if (behavior.equals("Crazy_Getter")){
        getInterval= Duration(5, SECONDS)
      }
      else if (behavior.equals("Slacker")){
        postInterval= Duration(30, SECONDS)
        getInterval= Duration(30, SECONDS)
      }
      context.system.scheduler.schedule(Duration(0,SECONDS), postInterval, self, CusPost)
      context.system.scheduler.schedule(Duration(10,SECONDS), getInterval, self, CusGet)
    }
    
    
    case CusPost => {      
      val PostIntructionPool = Seq[String]("/posts?title=GO&content=Gator", "/albums?name=Gator&intro=UF", "/albums/"+album+"/photos?name=nice")
      val url="http://127.0.0.1:8080/" + id + PostIntructionPool(Random.nextInt(PostIntructionPool.length))
      val fResponse = clientPipeline {
        Post(url)
      }
       fResponse onComplete {
        case Success(response) => 
          println("URL is " + url)
          println(response.entity.asString)
        case Failure(error) => println(error.getMessage)
      }      
    }
    
    case CusGet => {
      val GetIntructionPool = Seq[String]("", "?id&name&bod&type", "/posts", "/posts?id&title&content", "/albums", "/albums?id&name&intro&type", "/albums/photos", "/albums/photos?id&name&type", "/friendlist")
      val url="http://127.0.0.1:8080/" + id + GetIntructionPool(Random.nextInt(GetIntructionPool.length))
      val fResponse = clientPipeline {
        Get(url)
      }
       fResponse onComplete {
        case Success(response) => 
          println("URL is " + url)
          println(response.entity.asString)
        case Failure(error) => println(error.getMessage)
      }
    }
    
    
  }
}