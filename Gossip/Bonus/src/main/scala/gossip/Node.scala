import akka.actor.Actor
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import util.Random

case class loopGossip(message: String) //declaration for a message called loopGossip
case class loopPushSum(s: Double, w: Double) //declaration for a message called loopPS

class GossipNode(numNode: Int, topology: String, algo: String, nodeID: Int, moniter: ActorRef) extends Actor{
 
  private var ID: Int = nodeID
  private var isAlive: Boolean = false
  private var msgCount: Int = 0
  private var neighbours = new ListBuffer[Int];
  private var msg: String = null
  var roundCounter: Int = 0
  
  val duration1 = Duration(0, "s")
  val duration2 = Duration(1, "s")
  val roundScheduler = context.system.scheduler.schedule(duration1, duration2, self, "Spread")
  
  def receive = {
    case "Start" =>{
      //println("Start GenerateNeighbours -- (gossip)")
      GenerateNeighbours
    }
    
    case "Spread" => {
      roundCounter += 1
      if (isAlive == true) {
        SendMessageToRandomNeighbours
      }
    }
    
    case loopGossip(message: String) => {
      if (msgCount == 0) {
        isAlive = true
        msg = message
        msgCount += 1
        moniter ! start(roundCounter, ID)
      } 
      else if (msgCount == 9) {
        isAlive = false
      } 
      else {
        msg = message
        msgCount += 1
      }
    } 
    case "Are you alive?" =>
      sender ! "I am alive"
  }

  def SendMessageToRandomNeighbours = {
    //println("neighbour length : " + neighbours.length)
    var rand = Random.nextInt(neighbours.length)
    //println("rand number is : " + rand)
    //println("Self Node is : " + self.path + "|| Selected NOde is : "+neighbours.apply(rand).toString)
    var randneighbour = context.actorSelection("akka://GossipSystem/user/" + neighbours.apply(rand).toString)
    randneighbour ! loopGossip(msg)
  }
  
  def GetID(i: Int, j: Int, k: Int, l: Int): Int = { (i-1)+(j-1)*l+(k-1)*l*l }
  
  def GenerateNeighbours = {
    topology.toLowerCase() match {
      case "full" =>
        for (i <- 0 to numNode - 1) {
          if (i != ID)
            neighbours += i
        }
        
      case "line" =>
        if (ID - 1 >= 0)
          neighbours += ID - 1 
        if (ID + 1 < numNode)
          neighbours += ID + 1 

      case "3d" =>
        //(i,j,k)
        val length = Math.cbrt(numNode).toInt
        val k = ID/(length*length)+1
        val j = (ID - (k-1)*length*length)/length + 1 
        val i = ID - (k-1)*length*length - (j-1)*length +1
        //println("ID : "+ ID+ " length : " + length + " k : " + k + " j : " + j + " i : " + i)


        var track = 1
        while (track <= 6){
          //println("track number is : " + track)
          track match {
            case 1 =>{
              if ((i-1) < 1)  
                neighbours += GetID(length,j,k,length)
              else
                neighbours += GetID(i-1,j,k,length)
              track += 1
            }
            case 2 =>{     
              if ((j-1) < 1)  
                neighbours += GetID(i,length,k,length)
              else
                neighbours += GetID(i,j-1,k,length)
              track += 1
            }
            case 3 =>{     
              if ((k-1) < 1)  
                neighbours += GetID(i,j,length,length)
              else
                neighbours += GetID(i,j,k-1,length)
              track += 1
            }
            case 4 =>{     
              if ((i+1) > length)  
                neighbours += GetID(1,j,k,length)
              else
                neighbours += GetID(i+1,j,k,length)
              track += 1
            }
            case 5 =>{     
              if ((j+1) > 1)  
                neighbours += GetID(i,1,k,length)
              else
                neighbours += GetID(i,j+1,k,length)
              track += 1
            }
            case 6 =>{     
              if ((k+1) > 1)  
                neighbours += GetID(i,j,1,length)
              else
                neighbours += GetID(i,j,k+1,length)
              track += 1
            }
          }
        }
      //println("Self Node is : " + self.path + " || neighbours list : " + neighbours.toString)

        
      case "imp3d" =>
        //(i,j,k)
        val length = Math.cbrt(numNode).toInt
        val k = ID/(length*length)+1
        val j = (ID - (k-1)*length*length)/length + 1 
        val i = ID - (k-1)*length*length - (j-1)*length +1
        //println("ID : "+ ID+ " length : " + length + " k : " + k + " j : " + j + " i : " + i)
       var track = 1
        while (track <= 6){
          //println("track number is : " + track)
          track match {
            case 1 =>{
              if ((i-1) < 1)  
                neighbours += GetID(length,j,k,length)
              else
                neighbours += GetID(i-1,j,k,length)
              track += 1
            }
            case 2 =>{     
              if ((j-1) < 1)  
                neighbours += GetID(i,length,k,length)
              else
                neighbours += GetID(i,j-1,k,length)
              track += 1
            }
            case 3 =>{     
              if ((k-1) < 1)  
                neighbours += GetID(i,j,length,length)
              else
                neighbours += GetID(i,j,k-1,length)
              track += 1
            }
            case 4 =>{     
              if ((i+1) > length)  
                neighbours += GetID(1,j,k,length)
              else
                neighbours += GetID(i+1,j,k,length)
              track += 1
            }
            case 5 =>{     
              if ((j+1) > 1)  
                neighbours += GetID(i,1,k,length)
              else
                neighbours += GetID(i,j+1,k,length)
              track += 1
            }
            case 6 =>{     
              if ((k+1) > 1)  
                neighbours += GetID(i,j,1,length)
              else
                neighbours += GetID(i,j,k+1,length)
              track += 1
            }
          }
        }
        //add random node
        var rand = Random.nextInt(numNode)
        while (neighbours.contains(rand))
          rand = Random.nextInt(numNode)
        neighbours += rand

        //println("Self Node is : " + self.path + " || neighbours list : " + neighbours.toString)
    }
  }
}





class PushSumNode(numNode: Int, topology: String, algo: String, nodeID: Int, moniter: ActorRef) extends Actor {
  private var ID: Int = nodeID
  private var isAlive: Boolean = false
  private var neighbours = new ListBuffer[Int];
  private var s: Double = nodeID.toDouble
  private var w: Double = 1.0
  private var ratio: Double = s/w
  private var old_ratio: Double = s/w
  private var difference: Double = 0
  private var consecutive: Int = 0
  private var sumget: Int = 0
  var roundCounter: Int = 0
  
  val duration1 = Duration(0, "s")
  val duration2 = Duration(1, "s")
  val roundScheduler = context.system.scheduler.schedule(duration1, duration2, self, "Spread")
  
  def receive = {
    case "Start" =>{
      //println("Start GenerateNeighbours -- (pushsum)")
      GenerateNeighbours
    }
    
    case "Spread" => {
      roundCounter += 1
      //if node is alive, then spread sum
      if (isAlive == true) {
        SendSumToRandomNeighbours
      }
    }
    
    case loopPushSum(ss: Double, ww: Double) =>
      //start working(first time get command from main method)
      if (sumget == 0) {
        isAlive = true
        moniter ! start(roundCounter, ID) //tell moniter alive ID
        sumget += 1
        //println("Node " + self.path + " s : " + s + " w : " + w + " ss : " + ss + "ww : " + ww) 
        if (Calculate(ss, ww) < 10e-10)  consecutive += 1
        else  consecutive = 0
        //println("Node " + self.path + " || consecutive : " + consecutive)
      } else if (consecutive >= 3 && sumget > 0) {
        isAlive = false
        //println("Node " + self.path + " is dead")
      } else {
        isAlive = true
        if (Calculate(ss, ww) < 1)  consecutive += 1
        else  consecutive = 0
      }
    case "Are you alive?" =>
      sender ! "I am alive"
  }

  def Calculate(ss: Double, ww: Double): Double = {
    s = (s + ss)
    w = (w + ww)
    old_ratio = ratio
    ratio = s / w
    Math.abs(ratio - old_ratio)
  }
  
  def SendSumToRandomNeighbours = {
    var rand = Random.nextInt(neighbours.length)
    var randneighbour = context.actorSelection("akka://GossipSystem/user/" + neighbours.apply(rand).toString)
    randneighbour ! loopPushSum(s/2.0, w/2.0)
    s /= 2.0
    w /= 2.0
  }
  
  def GetID(i: Int, j: Int, k: Int, l: Int): Int = { (i-1)+(j-1)*l+(k-1)*l*l }
  
  def GenerateNeighbours = {
    topology.toLowerCase() match {
      case "full" =>
        for (i <- 0 to numNode - 1) {
          if (i != ID)
            neighbours += i
        }
        
      case "line" =>
        if (ID - 1 >= 0)
          neighbours += ID - 1 
        if (ID + 1 < numNode)
          neighbours += ID + 1 

      case "3d" =>
        //(i,j,k)
        val length = Math.cbrt(numNode).toInt
        val k = ID/(length*length)+1
        val j = (ID - (k-1)*length*length)/length + 1 
        val i = ID - (k-1)*length*length - (j-1)*length +1
        println("ID : "+ ID+ " length : " + length + " k : " + k + " j : " + j + " i : " + i)
        var track = 1
        //generate six neighbour
        while (track <= 6){
        track match {
          case 1 =>{
            if ((i-1) < 1)  
              neighbours += GetID(length,j,k,length)
            else
              neighbours += GetID(i-1,j,k,length)
            track += 1
          }
          case 2 =>{     
            if ((j-1) < 1)  
              neighbours += GetID(i,length,k,length)
            else
              neighbours += GetID(i,j-1,k,length)
            track += 1
          }
          case 3 =>{     
            if ((k-1) < 1)  
              neighbours += GetID(i,j,length,length)
            else
              neighbours += GetID(i,j,k-1,length)
            track += 1
          }
          case 4 =>{     
            if ((i+1) > length)  
              neighbours += GetID(1,j,k,length)
            else
              neighbours += GetID(i+1,j,k,length)
            track += 1
          }
          case 5 =>{     
            if ((j+1) > 1)  
              neighbours += GetID(i,1,k,length)
            else
              neighbours += GetID(i,j+1,k,length)
            track += 1
          }
          case 6 =>{     
            if ((k+1) > 1)  
              neighbours += GetID(i,j,1,length)
            else
              neighbours += GetID(i,j,k+1,length)
            track += 1
          }
        }
      }
        
      case "imp3d" =>
        //(i,j,k)
        val length = Math.cbrt(numNode).toInt
        val k = ID/(length*length)+1
        val j = (ID - (k-1)*length*length)/length + 1 
        val i = ID - (k-1)*length*length - (j-1)*length +1
        println("ID : "+ ID+ " length : " + length + " k : " + k + " j : " + j + " i : " + i)
        var track = 1
        while (track <= 6){
        track match {
          case 1 =>{
            if ((i-1) < 1)  
              neighbours += GetID(length,j,k,length)
            else
              neighbours += GetID(i-1,j,k,length)
            track += 1
          }
          case 2 =>{     
            if ((j-1) < 1)  
              neighbours += GetID(i,length,k,length)
            else
              neighbours += GetID(i,j-1,k,length)
            track += 1
          }
          case 3 =>{     
            if ((k-1) < 1)  
              neighbours += GetID(i,j,length,length)
            else
              neighbours += GetID(i,j,k-1,length)
            track += 1
          }
          case 4 =>{     
            if ((i+1) > length)  
              neighbours += GetID(1,j,k,length)
            else
              neighbours += GetID(i+1,j,k,length)
            track += 1
          }
          case 5 =>{     
            if ((j+1) > 1)  
              neighbours += GetID(i,1,k,length)
            else
              neighbours += GetID(i,j+1,k,length)
            track += 1
          }
          case 6 =>{     
            if ((k+1) > 1)  
              neighbours += GetID(i,j,1,length)
            else
              neighbours += GetID(i,j,k+1,length)
            track += 1
          }
        }
      }
        //add random node
        var rand = Random.nextInt(numNode)
        while (neighbours.contains(rand))
          rand = Random.nextInt(numNode)
        neighbours += rand

    }
  }
}