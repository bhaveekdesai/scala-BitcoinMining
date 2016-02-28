import akka.actor.{Props,ActorSystem, Actor}
import scala.util.Random

//Boss cases
case class beginMining(caller: String)
case class exposeBitcoin(message: String, bitcoinhash: String)
case object delegate

//Worker cases
case object fetchWork
case object keepMining
case class goFind(fetchedPrefixedZeroes: String, fetchedUFID: String, myID: Integer)

class Bitcoin(var message: String, var bitcoinhash: String) {}

object Main extends App {

  //initialize some variables
  var leadingZeroes = 0
  var prefixedZeroes = ""
  var UFID = "bhaveekdesai"
  var workerCount: Integer = 0

  //read commandline arguments
  if (args.length > 0) {
    if (args(0).forall(_.isDigit)) {
      leadingZeroes = args(0).toInt
      if (leadingZeroes <= 7) {
        for (i <- 1 to leadingZeroes) {
          prefixedZeroes += "0"
        }

        //create Boss actor
        val bitcoinMinersBoss = ActorSystem("BitcoinMinersBoss")
        val boss = bitcoinMinersBoss.actorOf(Props[Boss], name = "boss")

        //start the Boss
        boss ! beginMining("Boss")
      }
    } else {
        val remoteIP = args(0)

        //create worker
        val bitcoinMinersWorker = ActorSystem("BitcoinMinersWorker")
        val worker = bitcoinMinersWorker.actorOf(Props(new Worker(remoteIP)), name = "worker")

        //start Worker
        worker ! fetchWork
    }
  }

  def Miner(caller: String): Bitcoin = {
    //do magic
    var keepLooking = true
    var message, bitcoinhash: String = ""

    while (keepLooking) {
      message = UFID + ":" + caller + ":" + Random.alphanumeric.take(6).mkString
      bitcoinhash = java.security.MessageDigest.getInstance("SHA-256")
        .digest(message.getBytes("UTF-8"))
        .map("%02x".format(_)).mkString

      if (bitcoinhash.startsWith(prefixedZeroes)) keepLooking = false
    }

    val bitcoinFound = new Bitcoin(message, bitcoinhash)

    bitcoinFound
  }
}

class Boss extends Actor {
  def receive = {
    case `delegate` => {
      println("A worker joined")
      Main.workerCount += 1

      //send worker necessary details to start mining
      sender ! goFind(Main.prefixedZeroes, Main.UFID, Main.workerCount)
    }

    case exposeBitcoin(message, bitcoinhash) => {
      //display bitcoins
      println(message + " >> " +bitcoinhash)
    }

    case beginMining(caller) => {
      var fetchBitcoin = Main.Miner(caller)
      self ! exposeBitcoin(fetchBitcoin.message, fetchBitcoin.bitcoinhash)
      self ! beginMining(caller)
    }
  }
}

class Worker(remoteIP: String) extends Actor {

  //create remote actor
  val remotePath = "akka.tcp://BitcoinMinersBoss@"+remoteIP+":8083/user/boss"
  val remoteBoss = context.actorSelection(remotePath)
  var myName = "Worker"

  def receive = {
    case `fetchWork` => {
      //ask Boss to give details in order to start mining
      remoteBoss ! delegate
    }

    case goFind(fetchedPrefixedZeroes, fetchedUFID, myID) => {
      Main.prefixedZeroes = fetchedPrefixedZeroes
      Main.UFID = fetchedUFID
      myName = myName.concat(myID.toString)

      //start mining
      self ! keepMining
    }

    case `keepMining` => {
      var bitcoin = Main.Miner(myName)

      //hand over the mined bitcoin to Boss & keep digging
      remoteBoss ! exposeBitcoin(bitcoin.message, bitcoin.bitcoinhash)
      self ! keepMining
    }
  }
}