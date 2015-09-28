//package week03
import akka.actor.Actor
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef
import akka.actor.ActorSystem
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable.Map
import util.Random

case class gossipM(message: String) //declaration for a message called loopGossip
case class pushSum(s: Double, w: Double) //declaration for a message called loopPS
case class DieIfo(neighbourID: Int)
case class Map(partnerMap: scala.collection.immutable.Map[Int, Int])
case class Set_s(svalue: Double)
case object Wakeup
case object PSWakeup

class Node(numNode: Int, topology: String, algo: String, nodeID: Int, moniter: ActorRef) extends Actor{
  
  private var ID: Int = nodeID
  private var msgCount: Int = 0
  private var neighbours = new ListBuffer[Int]
  private var msg: String = null
  private var map: scala.collection.immutable.Map[Int, Int] = null
  
  var s: Double = 0.0
  var w: Double = 1.0
  var lastratio: Double = 0.0
  var ratio: Double = 0.0
  var consecutive: Int = 0
  
  
  def receive = {
    case "Start" =>{
      GenerateNeighbours
    }
    case Map(partnerMap: scala.collection.immutable.Map[Int, Int]) =>{
      map = partnerMap
    }
    case DieIfo(neighbourID: Int) =>{
      neighbours = neighbours.filter(_ != neighbourID)
    }
    case gossipM(message: String) => {
      if (msgCount == 0){
        //println("Wake UP : " + ID + " || Wake msg : " + msgCount)
        moniter ! "Wake Up"
      }
      msg = message
      msgCount += 1
      self ! Wakeup
    }
    case Wakeup =>{
       if (msgCount < 10  && neighbours.length > 0) {
         SendMessageToRandomNeighbours
         val duration = Duration(0, "s")
         context.system.scheduler.scheduleOnce(duration, self, Wakeup)
        }
        else if (msgCount >= 10  && neighbours.length > 0){
            for (i <- 0 to neighbours.length-1){
              val neighbour = context.actorFor("akka://GossipSystem/user/" + neighbours(i).toString)
              neighbour ! DieIfo(ID)
            }
            moniter ! finish(ID)
            context.stop(self)
          } 

        else if(msgCount >= 10 && neighbours.length == 0){
            moniter ! finish(ID)
            context.stop(self)
          }
        else{
          moniter ! finish(ID)
          context.stop(self)
        }
    }
    case pushSum(ss: Double, ww: Double) => {
      if (s == ID && w == 1.0)
        moniter ! "Wake Up"
      if (ss == 0.0 && ww == 0.0)
        self ! PSWakeup
      else {
        s = (s + ss) / 2
        w = (w + ww) / 2
        ratio = s / w
        self ! PSWakeup
      }
    }
    case PSWakeup => {
      if (Math.abs(ratio - lastratio) < 10e-10)
        consecutive += 1 
      else
        consecutive = 0
      
      lastratio = ratio
      
      if(consecutive < 3 && neighbours.length > 0 ) {
        SendPushToRandomNeighbours
        val duration = Duration(0, "s")
        context.system.scheduler.scheduleOnce(duration, self, PSWakeup)
      }
      else{
        for (i <- 0 to neighbours.length-1){
            val neighbour = context.actorFor("akka://GossipSystem/user/" + neighbours(i).toString)
            neighbour ! DieIfo(ID)
        }
        moniter ! finish(ID)
        context.stop(self)
      }
    }
    case Set_s(svalue: Double) =>
       s = svalue 
  }

  def SendMessageToRandomNeighbours = {
    if(neighbours.length > 0){
      var rand = Random.nextInt(neighbours.length)
      //println("Current NOde : " +  self.path + " || Neighbour List Lenght : " + neighbours.length + " || Rand : "  + rand)
      var randneighbour = context.actorSelection("akka://GossipSystem/user/" + neighbours(rand).toString)
      randneighbour ! gossipM(msg)
    }
  }
  def SendPushToRandomNeighbours = {
    if(neighbours.length > 0){
      var rand = Random.nextInt(neighbours.length)
      //println("Current NOde : " +  self.path + " || Neighbour List Lenght : " + neighbours.length + " || Rand : "  + rand)
      var randneighbour = context.actorSelection("akka://GossipSystem/user/" + neighbours(rand).toString)
      randneighbour ! pushSum(s/2, w/2)
      s /= 2
      w /= 2
    }
  }
  
  def GetID(i: Int, j: Int, k: Int, l: Int): Int = (i-1)+(j-1)*l+(k-1)*l*l
  
  def GenerateNeighbours = {
    topology.toLowerCase() match {
      case "full" => {
        for (i <- 0 to numNode - 1) {
          if (i != ID)  neighbours += i
        }
      }
      case "line" => {
        if (ID - 1 >= 0)  neighbours += ID - 1 
        if (ID + 1 < numNode)  neighbours += ID + 1 
      }
      case "3d" => {
        //(i,j,k)
        val length = Math.cbrt(numNode).toInt
        val k = ID/(length*length)+1
        val j = (ID - (k-1)*length*length)/length + 1 
        val i = ID - (k-1)*length*length - (j-1)*length +1
        var n = -1
        println("ID : "+ ID + " k : " + k + " j : " + j + " i : " + i)
        var track = 1
        while (track <= 6){
          track match {
            case 1 => { 
              if ((i-1) < 1) {
                n = GetID(length,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else{
                n = GetID(i-1,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              n = -1
              track += 1
            }
            case 2 => {     
              if ((j-1) < 1) {
                n = GetID(i,length,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              } 
              else{
                n = GetID(i,j-1,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              n = -1
              track += 1
            }
            case 3 => {     
              if ((k-1) < 1) { 
                n = GetID(i,j,length,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                n = GetID(i,j,k-1,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              n = -1
              track += 1
            }
            case 4 => {     
              if ((i+1) > length) {
                n = GetID(1,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                n = GetID(i+1,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              n = -1
              track += 1
            }
            case 5 => {     
              if ((j+1) > length) {  
                n = GetID(i,1,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                n = GetID(i,j+1,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              n = -1
              track += 1
            }
            case 6 => {     
              if ((k+1) > length) {
                n = GetID(i,j,1,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                n = GetID(i,j,k+1,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              n = -1
              track += 1
            }
          }
        }
        println("Self Node is : " + self.path + " || neighbours list : " + neighbours.toString)
      }
      case "imp3d" => {
        //(i,j,k)
        val length = Math.cbrt(numNode).toInt
        val k = ID/(length*length)+1
        val j = (ID - (k-1)*length*length)/length + 1 
        val i = ID - (k-1)*length*length - (j-1)*length +1
        //println("ID : "+ ID+ " length : " + length + " k : " + k + " j : " + j + " i : " + i)
        var track = 1
        while (track <= 6){
          track match {
            case 1 => { 
              if ((i-1) < 1) {
                val n: Int = GetID(length,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else{
                val n: Int = GetID(i-1,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              track += 1
            }
            case 2 => {     
              if ((j-1) < 1) {
                val n: Int = GetID(i,length,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              } 
              else{
                val n: Int = GetID(i,j-1,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              track += 1
            }
            case 3 => {     
              if ((k-1) < 1) { 
                val n: Int = GetID(i,j,length,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                val n: Int = GetID(i,j,k-1,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              track += 1
            }
            case 4 => {     
              if ((i+1) > length) {
                val n: Int = GetID(1,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                val n: Int = GetID(i+1,j,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              track += 1
            }
            case 5 => {     
              if ((j+1) > length) {  
                val n: Int = GetID(i,1,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                val n: Int = GetID(i,j+1,k,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              track += 1
            }
            case 6 => {     
              if ((k+1) > length) {
                val n: Int = GetID(i,j,1,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              else {
                val n: Int = GetID(i,j,k+1,length)
                if (!neighbours.contains(n) && n != ID)  neighbours += n
              }
              track += 1
            }
          }
        }
        /////
        if (map.contains(ID))
          neighbours += map(ID)
        println("Self Node is : " + self.path + " || neighbours list : " + neighbours.toString)
      }
    } 
  }
}

