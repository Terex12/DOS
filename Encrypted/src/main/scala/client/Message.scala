package client

import akka.actor.ActorRef

case class Register(name: String, bod: String, s: ActorRef)
case class FinishAll(s: ActorRef)
case object Done
case object Initialize
case object StartPost
case object FinishIniPost
case object Simu
case class Error(e: String)
case object CusPost
case object CusGet

case object RegisterDone
case class PreRegister(se: ActorRef)
case object Regi
case object InitializeDone

case object GetAES
case object GetAESDone
case class AESKey(se: ActorRef)

case object TestGet