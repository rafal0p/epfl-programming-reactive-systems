package actorbintree

import actorbintree.BinaryTreeSet.{Contains, ContainsResult, Insert, OperationFinished, Remove}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike, Matchers}

import scala.concurrent.duration.Duration
import scala.util.Random

//noinspection NameBooleanParameters
class MyBinaryTreeSuite(_system: ActorSystem) extends TestKit(_system) with FunSuiteLike with Matchers with BeforeAndAfterAll with ImplicitSender {

  def this() = this(ActorSystem("BinaryTreeSuite"))

  private val topNode = system.actorOf(Props[BinaryTreeSet])

  override def afterAll: Unit = {
    concurrent.Await.result(system.terminate(), Duration.Inf)
    ()
  }

  test("empty set doesn't contain positive number") {
    val id = Random.nextInt()

    topNode ! Contains(testActor, id, 5)

    expectMsg(ContainsResult(id, false))
  }

  test("empty set doesn't contain negative number") {
    val id = Random.nextInt()

    topNode ! Contains(testActor, id, -5)

    expectMsg(ContainsResult(id, false))
  }

  test("empty set doesn't contain zero") {
    val id = Random.nextInt()

    topNode ! Contains(testActor, id, 0)

    expectMsg(ContainsResult(id, false))
  }

  test("inserting is acknowledged") {
    val id = Random.nextInt()

    topNode ! Insert(testActor, id, 5)

    expectMsg(OperationFinished(id))
  }

  test("positive element is accessible after insert") {
    val id = Random.nextInt()
    val toInsert = 5
    ignoreMsg { case OperationFinished(_) => true }

    topNode ! Insert(testActor, Random.nextInt(), toInsert)
    topNode ! Contains(testActor, id, toInsert)

    expectMsg(ContainsResult(id, true))
  }

  test("negatvie element is accessible after insert") {
    val id = Random.nextInt()
    val toInsert = -5
    ignoreMsg { case OperationFinished(_) => true }

    topNode ! Insert(testActor, Random.nextInt(), toInsert)
    topNode ! Contains(testActor, id, toInsert)

    expectMsg(ContainsResult(id, true))
  }

  test("zero element is accessible after insert") {
    val id = Random.nextInt()
    val toInsert = 0
    ignoreMsg { case OperationFinished(_) => true }

    topNode ! Insert(testActor, Random.nextInt(), toInsert)
    topNode ! Contains(testActor, id, toInsert)

    expectMsg(ContainsResult(id, true))
  }

  test(" removing zero element") {
    val id = Random.nextInt()
    val elem = 0
    ignoreMsg { case OperationFinished(_) => true }

    topNode ! Insert(testActor, Random.nextInt(), elem)
    topNode ! Remove(testActor, Random.nextInt(), elem)
    topNode ! Contains(testActor, id, elem)

    expectMsg(ContainsResult(id, false))
  }
}
