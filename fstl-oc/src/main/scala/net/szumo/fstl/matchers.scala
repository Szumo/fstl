package net.szumo.fstl

import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.immutable.WrappedString

protected final class Node {
  var outputs = Set.empty[String]
  var transitions = Map.empty[Char, Node]
  var fail: Node = null
  def addOutputs(o: Seq[String]) { outputs = outputs ++ o }
  def size: Int = 1 + transitions.values.toSet[Node].map(t => t.size).sum
}

protected object Node {
  @tailrec
  def makeChild(node: Node, s: WrappedString, caseType: CaseType):Node = {
    val c = s.head
    val result = node.transitions.getOrElse(c, {
      val newNode = new Node
      node.transitions = node.transitions ++ caseType.variants(c).map(x => (x, newNode))
      newNode
    })
    val tail = s.tail
    if (tail.isEmpty) result else makeChild(result, tail, caseType)
  }
  @tailrec
  def findExisting(node:Node, s:WrappedString):Node = {
    if (s.isEmpty) node else {
      var current = node
      try {
        for (c <- s) {
          current = current.transitions(c)
        }
      } catch {
        case _:Exception => current = node
      }
      if (current != node) current else findExisting(node, s.tail)
    }
  }
}

protected class StringMatcherImpl[Output](words: Iterable[String], caseType: CaseType, resultFunc: String => Output) extends StringMatcher[Output] {
  private def join(strings: Iterable[String]) = strings.flatMap(s => s.toIterator).toIterator
  def isMatch(s: String) = !(new State(s.toIterator).isEmpty)
  def isMatch(s: Iterable[String]) = !(new State(join(s)).isEmpty)
  def apply(s: String):Iterator[Output] = new State(s.toIterator)
  def apply(s: Iterable[String]): Iterator[Output] = new State(join(s))
  def size = root.size

  def makeRoot(words: Iterable[String], caseType: CaseType, resultFunc: String => Output):Node = {
    val root:Node = new Node()
    root.fail = root
    for (word <- words) {
      Node.makeChild(root, word, caseType).addOutputs(Seq(word))
    }
    // set suffixes
    val queue = mutable.Queue.empty[(String, Node)]
    root.transitions.values.foreach( t => t.fail = root)
    queue ++= root.transitions.map( t => (t._1.toString, t._2))
    while (queue.nonEmpty) {
      val (prefix, current) = queue.dequeue()
      for ( (a, node) <- current.transitions) {
        if (node.fail == null) {
          val s = prefix+a
          queue.enqueue( (s, node) )
          val fail = Node.findExisting(root, s.tail)
          assert(fail != node)
          node.fail = fail
          node.outputs = node.outputs ++ fail.outputs
        }
      }
    }
    root
  }

  val root = makeRoot(words, caseType, resultFunc)

  class State(val chars: Iterator[Char]) extends Iterator[Output] {
    val lastFound = mutable.Queue.empty[Output]
    var node = root
    def hasNext: Boolean = {
      findNext()
      lastFound.nonEmpty
    }
    def findNext() {
      if (lastFound.isEmpty && chars.hasNext) {
        var c = chars.next()
        var continue = true
        do {
          node.transitions.get(c) match {
            case Some(x) => {
              node = x
              if (node.outputs.nonEmpty) {
                continue = false
              } else {
                if (chars.hasNext) c = chars.next() else continue = false
              }
            }
            case None => if (node.fail == node) {
              if (chars.hasNext) c = chars.next() else continue = false
            } else {
              node = node.fail
            }
          }
        } while (continue)
        lastFound ++= node.outputs.map(resultFunc)
      }
    }
    def next(): Output = {
      findNext()
      lastFound.dequeue()
    }
  }
  override def toString:String = "StringMatcherImpl"

}