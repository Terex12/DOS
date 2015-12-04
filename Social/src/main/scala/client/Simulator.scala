package client

import util.Random
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.util.Timeout
//inform server finish
  
class Simulator(firstNamePool: List[String], lastNamePool: List[String], behaviorPool: List[String], birthdayPool: List[String], num: Int) extends Actor{
    var index = 0
    def receive = {
      case Initialize => {  //let all client to register itself to server
        println("Enter Intialize")
        for (i <- 1 to num){
          val client = context.actorOf(Props(new SingleClient(behaviorPool(Random.nextInt(behaviorPool.length)))), i.toString())
          val name = firstNamePool(Random.nextInt(firstNamePool.length))+lastNamePool(Random.nextInt(lastNamePool.length))
          val bod = birthdayPool(Random.nextInt(birthdayPool.length))
          client ! Register(name, bod)
          if (i == num){
            Thread.sleep(100 * num)
            client ! FinishAll(self)
          }
        }
      }
      
      case Done => {  //send message to client to post before simulation
        for (i <- 1 to num){
          val client = context.actorFor("akka://ClientSimulator/user/simu/" + i.toString())
          client ! StartPost
        }
      }
      case FinishIniPost => {
        index += 1
        if (index == num){
          println("************ Start to Simulate *************")
          Thread.sleep(100*num)
          for (i <- 1 to num){
            val client = context.actorFor("akka://ClientSimulator/user/simu/" + i.toString())
            client ! Simu
          }
        }
      }
      case Error(e) => {
        println(e)
      }
    }
  }