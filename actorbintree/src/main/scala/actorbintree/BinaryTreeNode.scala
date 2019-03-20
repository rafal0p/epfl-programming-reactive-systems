package actorbintree

import actorbintree.BinaryTreeSet.{Contains, ContainsResult, Insert, OperationFinished}
import akka.actor.{Actor, ActorRef, Props}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  // optional
  def receive: Receive = normal

  // optional

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case contains: Contains => contains.elem match {
      case theSame if theSame == elem => contains.requester ! ContainsResult(contains.id, !removed)
      case greater if greater > elem => findIn(Right, contains)
      case smaller if smaller < elem => findIn(Left, contains)
    }
    case insert: Insert => insert.elem match {
      case theSame if theSame == elem => ()
      case greater if greater > elem => addTo(Right, insert)
      case smaller if smaller < elem => addTo(Left, insert)
    }
  }

  def addTo(position: Position, insert: Insert): Unit = {
    subtrees = subtrees.updated(position, context.actorOf(props(insert.elem, false)))
    insert.requester ! OperationFinished(insert.id)
  }

  def findIn(position: Position, contains: Contains): Unit = {
    if (subtrees.contains(position))
      subtrees(position) ! contains
    else
      contains.requester ! ContainsResult(contains.id, false)
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???


}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}