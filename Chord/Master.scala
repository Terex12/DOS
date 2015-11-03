package chord

import akka.actor.ActorSystem
import akka.actor.Props

import scala.util.Random
import scala.collection.mutable.ListBuffer
import java.security.{SecureRandom, MessageDigest}

object Master{
  
//  def generateIP: String = {
//    val IP: Array[Int] = new Array[Int](4)
//    for (i <- 0 to 3){
//      IP(i) = Random.nextInt(256)
//    }
//    IP.toList.mkString(".")
//  }
//  def sha1(IP: String): String = {
//    val md = MessageDigest.getInstance("SHA1");
//    md.update(IP.getBytes("UTF-8")); // Change this to "UTF-16" if needed
//    val digest = md.digest()
//    val hexString = new StringBuffer();
//    for ( j <- 0 to digest.length-1) {
//      val hex = Integer.toHexString(0xff & digest(j))
//      if(hex.length() == 1)  hexString.append('0')  //hex number must be
//      hexString.append(hex);
//    }
//    hexString.toString()
//  } 

  def main(args: Array[String]){
    if (args.length == 2) {
      val numNodes = Integer.parseInt(args(0))
      println("Number of Node is : " + numNodes)
      val numRequests = Integer.parseInt(args(1))
      println("Number of Request is : " + numRequests)
      val system = ActorSystem("c")
      val moniter = system.actorOf(Props(new Moniter(numNodes, numRequests, system)), name = "moniter")
      
      //Initialize network
      val currentNode: ListBuffer[Long] = new ListBuffer[Long]()
      
      
      
      //CHANGE M HERE******************************************
      val m = 5
     
     
     
      for (i <- 1 to numNodes){
        //val IP: String = generateIP
        //hash code result have 
        //val NID: Long = (IP.hashCode()+Math.pow(2, 31)-1).toLong
        var NID: Long = Random.nextInt(Math.pow(2,m).toInt).toLong
        while(currentNode.exists(_ == NID)){
          NID = Random.nextInt(Math.pow(2,m).toInt).toLong
        }
        
        //println("NID :" + NID)
        
        val predecessor = NID
        val successor = NID
        var FingerTable: List[Array[Long]] = List()
        //Initialize FingerTable
        //----------------------------->>
        for (i <- m to 0 by -1){
          if (i == 0){
            //first line all -1s
            val entry: Array[Long] = Array(-1.toLong, -1.toLong, -1.toLong, -1.toLong)
            FingerTable = entry :: FingerTable
          }
          else{
            val entry: Array[Long] = new Array[Long](4)
            val start = (NID + Math.pow(2, i-1)) % Math.pow(2, m)
            val startplus = (NID + Math.pow(2, i)) % Math.pow(2, m)
            entry(0) = start.toLong
            entry(1) = start.toLong
            entry(2) = startplus.toLong
            FingerTable = entry :: FingerTable
          }
        }
        
        //Build network
        if (currentNode.isEmpty) {
		  println("***************************************************")
          println("NODE:" + NID + " is initializing")
          currentNode += NID
          //----------------------------->>
          for (i <- 1 to m){
            val entry : Array[Long] = FingerTable(i)
            entry(3) = NID
          }
          val nodeActor = system.actorOf(Props(new Node(NID,predecessor,successor,FingerTable, moniter)), name = NID.toString())
          Thread.sleep(20000)
        }
        else {
          //select one random node
          val r = currentNode(Random.nextInt(currentNode.size))
          currentNode += NID
          val nodeActor = system.actorOf(Props(new Node(NID,predecessor,successor,FingerTable, moniter)), name = NID.toString())
          val n_dash = system.actorFor("akka://c/user/" + r.toString)
         
          //init_finger_table()
          println("***************************************************")
          println("NODE:" + NID + " is initializing")
          nodeActor ! init_finger_table(n_dash, 1)
          Thread.sleep(i*20000)
        }   
      }
      
      Thread.sleep(10000)
      for (i <- 0 to currentNode.size-1){
        println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
        val node = system.actorFor("akka://c/user/" + currentNode(i).toString())
        node ! print()
        Thread.sleep(1000)
      }
      
	  		
      //start finding resource
      println("%%%%%%%%%%%%%%%%%%%%%%% START FIND SOURCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      for (i <- 0 to currentNode.size-1){
        val node = system.actorFor("akka://c/user/" + currentNode(i).toString())
        for (j <- 1 to numRequests) {
          node ! "Find Resource"
          Thread.sleep(1000)
        }
      }
      

	  		
	  //FAIL NODES <BONUS> ******************************************************************************
      
      
      //Pick a random node, fail it
      val f = currentNode(Random.nextInt(currentNode.size))
      currentNode -= f
      println("//////////////////////////////////////////////////////////////////")
      println("I am NODE " + f + ". I have failed")
      println("//////////////////////////////////////////////////////////////////")
      val fail_ref = system.actorFor("akka://c/user/" + f.toString)
      fail_ref ! leave()
      Thread.sleep(numNodes*20000)
      
      //Print finger tables
      Thread.sleep(10000)
      for (i <- 0 to currentNode.size-1){
        println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
        val node = system.actorFor("akka://c/user/" + currentNode(i).toString())
        node ! print()
        Thread.sleep(1000)
      }
      
      	
      //start finding resource
      println("%%%%%%%%%%%%%%%%%%%%%%% START FIND SOURCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      for (i <- 0 to currentNode.size-1){
        val node = system.actorFor("akka://c/user/" + currentNode(i).toString())
        for (j <- 1 to numRequests) {
          node ! "Find Resource"
          Thread.sleep(1000)
        }
      }
      
      
      
      Thread.sleep(1000)
      system.shutdown()

    }
    else
      println("Check your input")
  }
}
