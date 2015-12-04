package client

import akka.actor.ActorRef

case class Register(name: String, bod: String)
case class FinishAll(s: ActorRef)
case object Done
case object Initialize
case object StartPost
case object FinishIniPost
case object Simu
case class Error(e: String)
case object CusPost
case object CusGet