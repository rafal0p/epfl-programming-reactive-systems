package actorbintree

import actorbintree.BinaryTreeSet.{Contains, ContainsResult}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import scala.concurrent.duration.Duration
import scala.util.Random

class MyBinaryTreeSuite(_system: ActorSystem) extends TestKit(_system) with FunSuiteLike with Matchers with BeforeAndAfterAll with ImplicitSender {

  def this() = this(ActorSystem("BinaryTreeSuite"))

  override def afterAll: Unit = {
    concurrent.Await.result(system.terminate(), Duration.Inf)
    ()
  }

  test("empty set doesn't contain anything") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    val id = Random.nextInt()
    topNode ! Contains(testActor, id, 0)

    expectMsg(ContainsResult(id, false))
  }
}
