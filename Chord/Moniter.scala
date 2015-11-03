package chord

import akka.actor.ActorSystem
import akka.actor.Actor

case class finish(hop: Int)  //start a moniter
case class check()

class Moniter(numNode: Int, numRequest: Int, system: ActorSystem) extends Actor {
  var sum = 0;
  var times = 0;
  var fail = 0;
  def receive = {
    case finish(hop: Int) => {
      times += 1
      sum += hop
      if (fail==0 && times==numNode*numRequest){
		println("//////////////////////////////////////////////////////////////////")
		println(" Average Hop : " + sum.toFloat/(numNode*numRequest))
		println("//////////////////////////////////////////////////////////////////")
		sum = 0
		times = 0
		fail = 1
	  }
	  //one node failed
	  if (fail==1 && times==((numNode-1)*numRequest)){
		println("//////////////////////////////////////////////////////////////////")
		println(" Average Hop After Fail : " + sum.toFloat/((numNode-1)*numRequest))
		println("//////////////////////////////////////////////////////////////////")
		terminate
	  }        
    }
  }
  def terminate = {
    //println("//////////////////////////////////////////////////////////////////")
    //println(" Average Hop : " + sum.toFloat/(numNode*numRequest))
    system.shutdown
  }
}
