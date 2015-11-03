package chord

import akka.actor.Actor
import akka.actor.ActorRef

import scala.collection.mutable.Set
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.util.control._

case class fs_find_predecessor(KID: Long, Original: ActorRef, Current: ActorRef, Hop: Int)
case class if_find_predecessor(KID: Long, Original: ActorRef, Current: ActorRef, Index: Int, Ori_n_dash: ActorRef)
case class up_find_predecessor(KID: Long, Original: ActorRef, Current: ActorRef, Index: Int)

case class fs_cloest_preceding_finger(KID: Long, Original: ActorRef, N: Node, Hop: Int)
case class if_cloest_preceding_finger(KID: Long, Original: ActorRef, N: Node, Index: Int, Ori_n_dash: ActorRef)
case class up_cloest_preceding_finger(KID: Long, Original: ActorRef, N: Node, Index: Int)

case class fs_result_of_predecessor(n_dash: Node, Hop: Int, Kid: Long)
case class if_result_of_predecessor(n_dash: Node, Index: Int, Ori_n_dash: ActorRef)
case class up_result_of_predecessor(n_dash: Long, Index: Int)

case class init_finger_table(n_dash: ActorRef, Index: Int)
case class if_find_successor(StartNode: Long, Original: ActorRef, Index: Int, Ori_n_dash: ActorRef)
case class set_predecessor(n: Long)
case class set_successor(n: Long) 
case class result_set_predecessor(Predecessor: Long) 
case class finish_init_finger(Index: Int)
case class update_others()
case class update_finger_table(N: Long, Index: Int)
case class query_predecessor(Original: ActorRef, Index: Int, Ori_n_dash: ActorRef)

case class print()

case class leave()
case class fail_find_predecessor(originalNID: Long, originalSuccNID: Long, kid: Long, original: ActorRef, current: ActorRef, i: Int) 
case class fail_cloest_preceding_finger(originalNID: Long, originalSuccNID: Long, kid: Long, original: ActorRef, node: Node, i: Int) 
case class fail_result_of_predecessor(originalNID: Long, originalSuccNID: Long, n_dash: Long, i: Int)
case class remove_node(originalNID: Long, originalSuccNID: Long, i: Int)


class Node(nodeID: Long, pre: Long, suc: Long, finger: List[Array[Long]], moniter: ActorRef) extends Actor{
  
  private var NID:Long = nodeID
  private var FingerTable:List[Array[Long]] = finger
  private var successor: Long = suc
  private var predecessor: Long = pre
  private var counter = 0
  
  //CHANGE M HERE******************************************
  private val m = 5
  
  //val duration = Duration(1, "s")
  //context.system.scheduler.scheduleOnce(duration, self, "Find Resource")

  def find_source(id: Long) ={
    val hop = 0
    // id == n
    if (this.NID == id) {
      println("I am Node : " + NID + ", I am finding KID : " + id)
      println("Find it")
      println("Result : " + this.NID)
      println("Hop is : " + hop)
      moniter ! finish(hop)
    }
    else
      self ! fs_find_predecessor(id, self, self, hop)
  }
  def receive = {
    //============================================= code for searching =================================================
    case "Find Resource" => {
      //Random generate KID
      val KID = Random.nextInt(Math.pow(2,m).toInt).toLong;
      find_source(KID)
    }
    case fs_find_predecessor(kid: Long, original: ActorRef, current: ActorRef, hop: Int) => {
      var hhop  = hop+1
      val n_dash = this
      val n_dash_actor = current
      if (n_dash.NID == n_dash.successor)
        original ! fs_result_of_predecessor(n_dash, hhop, kid)
      else{
        if ((n_dash.NID < n_dash.successor && (kid > n_dash.NID && kid <= n_dash.successor)) || (n_dash.NID > n_dash.successor && (kid>n_dash.NID || kid<=n_dash.successor))) 
          original ! fs_result_of_predecessor(n_dash, hhop, kid)
        else
          n_dash_actor ! fs_cloest_preceding_finger(kid, original, this, hhop)
      }
    }
    case fs_cloest_preceding_finger(kid: Long, original: ActorRef, node: Node, hop: Int) =>{
      val finger_table = node.FingerTable
      val loop = new Breaks
      loop.breakable{
        for (x <- finger_table.size-1 to 1 by -1){
          val entry : Array[Long] = finger_table(x)
          if ((node.NID > kid && (entry(3) > node.NID || entry(3) < kid)) || (node.NID < kid && entry(3) > node.NID && entry(3) < kid)){
            val next = context.actorFor("akka://c/user/" + entry(3).toString)
            next ! fs_find_predecessor(kid, original, next, hop)
            loop.break;
          }
        }
      }
    }
    case fs_result_of_predecessor(n_dash: Node, hop: Int, kid: Long) => {
      println("I am Node : " + NID + ", I am finding KID : " + kid)
      println("Find it")
      println("Result : " + n_dash.successor)
      println("Hop is : " + hop)
      moniter ! finish(hop)
    }
    
    //============================================= code for initializing =================================================
    case init_finger_table(ori_n_dash: ActorRef, index: Int) =>{
      //edit finger table line by line
      //println("This is NID : "+this.NID + " N_DASH : " + ori_n_dash.path)
      if (index == 1){
        //println("In the IF")
        ori_n_dash ! if_find_successor(this.FingerTable(1)(0), self, 1, ori_n_dash)
      }
        //----------------------------->>
      else if (index <= m){
        //println("else if")
        if ((NID < this.FingerTable(index-1)(3) && this.FingerTable(index)(0) >= NID && this.FingerTable(index)(0) < this.FingerTable(index-1)(3)) ||(NID > this.FingerTable(index-1)(3) && (this.FingerTable(index)(0) >= NID || this.FingerTable(index)(0) < this.FingerTable(index-1)(3)))){
          //println("In the range : [" + NID + " , "+this.FingerTable(index-1)(3) + ")")
          this.FingerTable(index)(3) = this.FingerTable(index-1)(3)
          self ! finish_init_finger(index)
          self ! init_finger_table(ori_n_dash, index+1)
        }
        else{
          //println("not in range")
          ori_n_dash ! if_find_successor(this.FingerTable(index)(0), self, index, ori_n_dash)
        }
      }
    }
    
    case if_find_successor(start: Long, original: ActorRef,i: Int, ori_n_dash: ActorRef) =>{ 
      self ! if_find_predecessor(start, original, self, i, ori_n_dash)
    }

    case if_find_predecessor(kid: Long, original: ActorRef, current: ActorRef, i: Int, ori_n_dash: ActorRef) =>{
      val n_dash = this
      val n_dash_actor = current
      
      if (kid == n_dash.NID){
        //return predecessor
        //println("Ndash ID == kid")
        context.actorFor("akka://c/user/" + n_dash.predecessor.toString()) ! query_predecessor(original, i, ori_n_dash)
        //original ! if_result_of_predecessor(n_dash.predecessor, i)
      }
      else{
        //May have problem
        if (n_dash.NID == n_dash.successor){
          //println("Ndash ID == ndash.succ")
          original ! if_result_of_predecessor(n_dash, i, ori_n_dash)
        }
        else{
          if ((n_dash.NID < n_dash.successor && (kid > n_dash.NID && kid <= n_dash.successor)) || (n_dash.NID > n_dash.successor && (kid>n_dash.NID || kid<=n_dash.successor))){
            //println("zaifanweili a~~~ ++ kid : " + kid + " (N_DASH : " + n_dash.NID + " NDASH_SEC : "+ n_dash.successor +")")
            original ! if_result_of_predecessor(n_dash, i, ori_n_dash)
          }
          else{ 
            //println("buzai a~~~ kid : " + kid + " (N_DASH : " + n_dash.NID + " NDASH_SEC : "+ n_dash.successor +")")
            n_dash_actor ! if_cloest_preceding_finger(kid, original, this, i, ori_n_dash)
          }
        }
      }
    }
    case query_predecessor(original: ActorRef, i: Int, ori_n_dash: ActorRef) => {
      original ! if_result_of_predecessor(this, i, ori_n_dash)
    }
    case if_cloest_preceding_finger(kid: Long, original: ActorRef, node: Node, i: Int, ori_n_dash: ActorRef) => {
      val finger_table = node.FingerTable
      val loop = new Breaks
      loop.breakable{
        for (x <- finger_table.size-1 to 1 by -1){
          val entry : Array[Long] = finger_table(x)
          if ((node.NID > kid && (entry(3) > node.NID || entry(3) < kid)) || (node.NID < kid && entry(3) > node.NID && entry(3) < kid)){
           //println("if ( "+ entry(3) +" (- (" + node.NID + " , " + kid+ " )")
            val next = context.actorFor("akka://c/user/" + entry(3).toString)
            next ! if_find_predecessor(kid, original, next, i, ori_n_dash)
            loop.break;
          }
        }
      }
    }
    
    case if_result_of_predecessor(n_dash: Node, i: Int, ori_n_dash: ActorRef) => {
      if (i == 1){
        //edit one entry in finger
        this.FingerTable(i)(3) = n_dash.successor
        self ! finish_init_finger(i)
        this.successor = this.FingerTable(i)(3)
        // set predecessor
        val successor = context.actorFor("akka://c/user/" + this.successor.toString)
        successor ! set_predecessor(this.NID)
        Thread.sleep(2000)
        //edit one entry then next
        self ! init_finger_table(ori_n_dash, i+1)
        
      }
      else{
        this.FingerTable(i)(3) = n_dash.successor
        self ! finish_init_finger(i)
        Thread.sleep(2000)
        self ! init_finger_table(ori_n_dash, i+1)
      }
    }

    case set_predecessor(n: Long) => {
      sender ! result_set_predecessor(this.predecessor)
      this.predecessor = n
    }
    
    case result_set_predecessor(pre: Long) => {
      this.predecessor = pre
    }
    
    case set_successor(n: Long) =>{
      this.successor = n
    }
    
    case finish_init_finger(index: Int) => {
      this.counter += 1
      val entry : Array[Long] = this.FingerTable(index)
      //--------------------------------->>
      if(counter == m){
       //we also need modify predecessor's successor
       val predecessor = context.actorFor("akka://c/user/" + this.predecessor.toString)
       predecessor ! set_successor(this.NID)
       Thread.sleep(2000)
       
        
       //check finger table
        //println("After self Initialzation ======>>>")
        //println("NID : " + NID)
        //println("Prodecessor : " + this.predecessor)
        //println("Sucessor : " + this.successor)
        //----------------------------->>
        for (i <- 1 to m){
          val entry : Array[Long] = this.FingerTable(i)
          //println("In Build  " + entry(0)  + "  " + entry(1) +"  "+ entry(2) + "  "+entry(3))
        }
        
        self ! update_others()
      }
    }
    
    //============================================= code for update others =================================================
    case update_others() =>{
      //println("In UP_other ==> just do once")
      this.counter = 0
      //----------------negative --------------->>
      for (i <- 1 to m){
        var kid = this.NID-Math.pow(2, i-1).toLong+1
        if (kid < 0){
          kid = kid + Math.pow(2,m).toLong
        }
        //println("In UP_other ==> kid : " + kid)
        //println("==================================")
        self ! up_find_predecessor(kid, self, self, i)
      }
    }
    
    case up_find_predecessor(kid: Long, original: ActorRef, current: ActorRef, i: Int) =>{
      val n_dash = this
      val n_dash_actor = current
      //println("In UP_find_prede ==> kid : " + kid)
      //println("In UP_find_prede ==> range : (" + n_dash.NID + " , " + n_dash.successor + "]")
      
      if (kid == n_dash.NID){
        //return predecessor
        original ! up_result_of_predecessor(n_dash.predecessor, i)
      }
      else{
        if (n_dash.NID == n_dash.successor){  
          //whole ring case
          original ! up_result_of_predecessor(n_dash.NID, i)
        }
        else{
          if ((n_dash.NID < n_dash.successor && (kid > n_dash.NID && kid <= n_dash.successor)) || (n_dash.NID > n_dash.successor && (kid>n_dash.NID || kid<=n_dash.successor))){
            //println("In UP_find_prede ==> in range")
            original ! up_result_of_predecessor(n_dash.NID, i)
          }
          else {
            //println("In UP_find_prede ==> not in range, to find in UPCPF")
            n_dash_actor ! up_cloest_preceding_finger(kid, original, this, i)
          }
        }
      }
    }
    case up_cloest_preceding_finger(kid: Long, original: ActorRef, node: Node, i: Int) => {
      //println("In UP_CPF ==> kid : " + kid)
      //println("In UP_CPF ==> NID : " + this.NID)
      //println("======================================================")
      val finger_table = node.FingerTable
      val loop = new Breaks
      loop.breakable{
        for (x <- finger_table.size-1 to 1 by -1){
          val entry : Array[Long] = finger_table(x)
          if ((node.NID > kid && (entry(3) > node.NID || entry(3) < kid)) || (node.NID < kid && entry(3) > node.NID && entry(3) < kid)){
            val next = context.actorFor("akka://c/user/" + entry(3).toString)
            next ! up_find_predecessor(kid, original, next, i)
            loop.break;
          }
        }
      }
    }
    
    case up_result_of_predecessor(n_dash: Long, i: Int) => {
      this.counter += 1
      val p = context.actorFor("akka://c/user/" + n_dash.toString())
      //println("In UP_result_of_pre ==> p : " + p.path)
      p ! update_finger_table(this.NID, i)
      //Thread.sleep(100000)
    }
    
    case update_finger_table(s: Long, i: Int) => {
      //println("In UP_FI_TA ==> n : " + this.NID + " s : " + s + " i : " + i)
      //some changes in range
      if (s != this.NID){
        if (this.NID == this.FingerTable(i)(3)){
          this.FingerTable(i)(3) = s
          val p = context.actorFor("akka://c/user/" + this.predecessor.toString())
          p ! update_finger_table(s, i)
        }
        else{
          if ((NID < this.FingerTable(i)(3) && s > NID && s <= this.FingerTable(i)(3)) ||(NID > this.FingerTable(i)(3) && (s > NID || s <= this.FingerTable(i)(3)))){
            this.FingerTable(i)(3) = s
            val p = context.actorFor("akka://c/user/" + this.predecessor.toString())
            p ! update_finger_table(s, i)
          }  
        }
      }  
    }
    
    case print() => {
      println("NID : " + NID)
      println("Prodecessor : " + this.predecessor)
      println("Sucessor : " + this.successor)
      //----------------------------->>
      for (i <- 1 to m){
        val entry : Array[Long] = this.FingerTable(i)
        println("In Build  " + entry(0)  + "  " + entry(1) +"  "+ entry(2) + "  "+entry(3))
      }
    }
 















   //============================================= code for failures =================================================   
    
    case leave() => {
      //println("In leave ==> just do once")
      //this.counter = 0
      
      if (this.NID != this.successor) { // it is not the last node
		  //Update predecesor and successor
		  var neighbor = context.actorFor("akka://c/user/" + this.predecessor.toString())
		  neighbor ! set_successor(this.successor)
		  neighbor = context.actorFor("akka://c/user/" + this.successor.toString())
		  neighbor ! result_set_predecessor(this.predecessor)
		  
		  //----------------negative --------------->>
		  for (i <- 1 to m){
			var kid = this.NID-Math.pow(2, i-1).toLong+1
			if (kid < 0){
			  kid = kid + Math.pow(2,m).toLong
			}
			//println("In leave ==> kid : " + kid)
			//println("==================================")
			self ! fail_find_predecessor(this.NID, this.successor, kid, self, self, i)
		  }
	  }
	  //Thread.sleep(50000)
	  //context.stop(self)
    }
    
    case fail_find_predecessor(originalNID: Long, originalSuccNID: Long, kid: Long, original: ActorRef, current: ActorRef, i: Int) =>{
      val n_dash = this
      val n_dash_actor = current
      //println("In fail_find_prede ==> kid : " + kid)
      //println("In fail_find_prede ==> range : (" + n_dash.NID + " , " + n_dash.successor + "]")
      
      if (kid == n_dash.NID){
        //return predecessor
        original ! fail_result_of_predecessor(originalNID, originalSuccNID, n_dash.predecessor, i)
      }
      else{
        //May have problem
        if (n_dash.NID == n_dash.successor){  //whole ring
          original ! fail_result_of_predecessor(originalNID, originalSuccNID, n_dash.NID, i)
        }
        else{
          if ((n_dash.NID < n_dash.successor && (kid > n_dash.NID && kid <= n_dash.successor)) || (n_dash.NID > n_dash.successor && (kid>n_dash.NID || kid<=n_dash.successor))){
            //println("In fail_find_prede ==> in range")
            original ! fail_result_of_predecessor(originalNID, originalSuccNID, n_dash.NID, i)
          }
          else {
            //println("In fail_find_prede ==> not in range, to find in failCPF")
            n_dash_actor ! fail_cloest_preceding_finger(originalNID, originalSuccNID, kid, original, this, i)
          }
        }
      }
      
    }
    case fail_cloest_preceding_finger(originalNID: Long, originalSuccNID: Long, kid: Long, original: ActorRef, node: Node, i: Int) => {
      //println("In fail_CPF ==> kid : " + kid)
      //println("In fail_CPF ==> NID : " + this.NID)
      //println("======================================================")
      val finger_table = node.FingerTable
      val loop = new Breaks
      loop.breakable{
        for (x <- finger_table.size-1 to 1 by -1){
          val entry : Array[Long] = finger_table(x)
          if ((node.NID > kid && (entry(3) > node.NID || entry(3) < kid)) || (node.NID < kid && entry(3) > node.NID && entry(3) < kid)){
            val next = context.actorFor("akka://c/user/" + entry(3).toString)
            next ! fail_find_predecessor(originalNID, originalSuccNID, kid, original, next, i)
            loop.break;
          }
        }
      }
      //println("failC__Not find in finger Table")
      //return n
      //sender ! fail_result_of_predecessor(this, i)
    }
    
    case fail_result_of_predecessor(originalNID: Long, originalSuccNID: Long, n_dash: Long, i: Int) => {
      //this.counter += 1
      val p = context.actorFor("akka://c/user/" + n_dash.toString())
      //println("In fail_result_of_pre ==> p : " + p.path)
      
      p ! remove_node(originalNID, originalSuccNID, i)
      
      //Thread.sleep(10000)
      
    }
    
    case remove_node(originalNID: Long, s: Long, i: Int) => {
      //println("In fail_FI_TA ==> n : " + this.NID + " s : " + s + " i : " + i)
      //some changes in range
      if (s != this.NID){
        if (this.FingerTable(i)(3)==originalNID){
          //println("Whole Ring !!!")
          this.FingerTable(i)(3) = s
          val p = context.actorFor("akka://c/user/" + this.predecessor.toString())
          p ! remove_node(originalNID, s, i)
        }
        /*
        else{
          if ((NID < this.FingerTable(i)(3) && s > NID && s <= this.FingerTable(i)(3)) ||(NID > this.FingerTable(i)(3) && (s > NID || s <= this.FingerTable(i)(3)))){
            this.FingerTable(i)(3) = s
            val p = context.actorFor("akka://c/user/" + this.predecessor.toString())
            p ! remove_node(originalNID, s, i)
          }  
        }
        */
      }  
    } 
    
  }
}
