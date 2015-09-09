package common

import java.security.MessageDigest
import scala.collection.mutable.Map

  sealed trait Message
  case object MiningRequest extends Message  
  //case class Work(k: Int, prefix: String, random: String) extends Message
  case class Work(k: Int, prefix: String, times: Int) extends Message
  case class RightResult(in: String, hashString: String) extends Message 
  case class WrongResult(in: String, hashString: String) extends Message 
  case class Coins(coins: Map[String, String]) extends Message
  case object Terminal extends Message
  case object Available extends Message
  case object Stop extends Message

  class helptool{
    //SHA256 hash function
    def sha256(prefix: String, random: String): String = {
      val md = MessageDigest.getInstance("SHA-256");
      val plaintext = prefix + ";" + random
      md.update(plaintext.getBytes("UTF-8")); // Change this to "UTF-16" if needed
      val digest = md.digest()
      val hexString = new StringBuffer();
      for ( j <- 0 to digest.length-1) {
        val hex = Integer.toHexString(0xff & digest(j))
        if(hex.length() == 1)  hexString.append('0')  //hex number must be
        hexString.append(hex);
      }
      hexString.toString()
    } 
    //check coins
    def judgeCoins(k: Int, hash: String) = {
      if (hash.startsWith("0" * k))
        true
      else
        false
    }
  }