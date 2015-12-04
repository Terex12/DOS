package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import spray.routing.{Route, HttpService, RequestContext}
import spray.http.MediaTypes._
import spray.httpx.Json4sSupport
import org.json4s.DefaultFormats


class Server extends Actor with Service {
  implicit def actorRefFactory = context
  def receive = runRoute(rout)
}

// service module
trait Service extends HttpService with Json4sSupport {
  val mmu = actorRefFactory.actorOf(Props[MMU], "Memory")
  val json4sFormats = DefaultFormats
  val rout = {
    get {
      path(Segments) { segments =>
        validate(segments.exists(_.matches("[0-9]*")), "unmatched path"){
          parameterSeq{ params =>
            val para = params
            val path = segments
            val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
            ctx => op ! GetRequest(path, para, ctx, mmu)
            //println("op dieing...")
            }
        }~
        validate(segments.exists(_.matches("me")), "unmatched path"){
          parameterSeq{ params =>
            val para = params
            val path = segments
            
            
            val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
            ctx => op ! MyGetRequest(path, para, ctx, mmu)
            //println("op dieing...")
         }
        }~
        validate(segments.exists(_.matches("registerdone")), "unmatched path"){
          parameterSeq{ params =>
            val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
            ctx => op ! RegisterDone(mmu, ctx)
            //println("op dieing...")
         }
        }
      }
    }~
    post{ 
      path(Segments) { segments =>
        validate(segments.exists(_.matches("[0-9]*")), "unmatched path"){
          parameterSeq{ params =>
            //println("Create a new Node")
            val para = params
            val path = segments
            val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
            ctx => op ! PostRequest(path, para, ctx, mmu)
            //println("op dieing...")
         }
        }~
        validate(segments.exists(_.matches("page")), "unmatched path"){
          parameterSeq{ params =>
            //println("Create a new user")
            val para = params
            val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
            ctx => op ! CreateUser(para, ctx, mmu)
            //println("op dieing...")
         }
        }
      }
    }
  }
}