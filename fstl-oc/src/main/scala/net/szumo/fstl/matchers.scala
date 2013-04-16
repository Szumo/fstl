package net.szumo.fstl

import scala.collection.mutable
import scala.annotation.tailrec

protected abstract class Node[Output] {
  val number = Node.number
  Node.number += 1
  var outputs = Set.empty[Output]
  var transitions = Map.empty[Char, Node[Output]]
  var fail: Node[Output] = null
  def longString = s"Node $number [$outputs] transitions $transitions fails to ${if (fail==null) "null" else fail.shortString}"
  def shortString = s"Node $number"
  override def toString = shortString
  def addOutputs(o: Seq[Output]) { outputs = outputs ++ o }
  def size: Int = 1 + transitions.values.toSet[Node[Output]].map(t => t.size).sum
}

protected class Root[Output] extends Node[Output] {
  fail = this
}

protected class Branch[Output] extends Node[Output]

protected object Node {
  var number = 1
  @tailrec
  def makeChild[Output](node: Node[Output], s: String, caseType: CaseType):Node[Output] = {
    val result = node.transitions.getOrElse(s.head, {
      val newNode = new Branch[Output]
      caseType(node, s.head, newNode)
      newNode
    })
    val tail = s.tail
    if (tail.isEmpty) result else makeChild(result, tail, caseType)
  }
  @tailrec
  def find[Output](node:Node[Output], c:Char):Node[Output] = {
    val result = node.transitions.get(c)
    //println(s"find: ${node.longString} -> ${result.map(x=>x.longString).getOrElse("")}")
    result match {
      case Some(x) => x
      case None => if (node.fail == node) node else find(node.fail, c)
    }
  }
  @tailrec
  def findExisting[Output](node:Node[Output], s:String):Node[Output] = {
    //println(s"findExisting: ${node.longString} $s")
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
  def apply[Output](words: Iterable[String], caseType: CaseType, resultFunc: String => Output):Node[Output] = {
    val root:Node[Output] = new Root[Output]()
    for (word <- words) {
      Node.makeChild(root, word, caseType).addOutputs(Seq(resultFunc(word)))
    }
    // set suffixes
    val queue = mutable.Queue.empty[(String, Node[Output])]
    root.transitions.values.foreach( t => t.fail = root)
    queue ++= root.transitions.map( t => (t._1.toString, t._2))
    while (queue.nonEmpty) {
      val (prefix, current) = queue.dequeue()
      for ( (a, node) <- current.transitions) {
        if (node.fail == null) {
        val s = prefix+a
          queue.enqueue( (s, node) )
          //println(s"Processing suffix for node $node $s")
          val fail = findExisting(root, s.tail)
          assert(fail != node)
          node.fail = fail
          node.outputs = node.outputs ++ fail.outputs
        }
      }
    }
    //println(root)
    root
  }
}

protected class StringMatcherImpl[Output,Result](words: Iterable[String], caseType: CaseType, resultFunc: String => Output,
                                       createResult: (Output, Int) => Result) extends StringMatcher[Result] {
  def isMatch(s: String) = !(new State(Iterator(s)).isEmpty)
  def isMatch(s: Iterable[String]) = !(new State(s.iterator).isEmpty)
  def apply(s: String):Iterator[Result] = new State(Iterator(s))
  def apply(s: Iterable[String]): Iterator[Result] = new State(s.iterator)
  def size = root.size

  val root = Node(words, caseType, resultFunc)

  class State(strings: Iterator[String]) extends Iterator[Result] {
    val chars: Iterator[Char] = strings.flatMap(s => s.toIterator)
    val lastFound = mutable.Queue.empty[Result]
    var position = 0
    var node = root
    def hasNext: Boolean = {
      tryFind()
      lastFound.nonEmpty
    }
    def tryFind() {
      while (lastFound.isEmpty && chars.hasNext) {
        position += 1
        node = Node.find(node, chars.next())
        if (node.outputs.nonEmpty) lastFound ++= node.outputs.map(o => createResult(o, position))
      }
    }
    def next(): Result = {
      tryFind()
      lastFound.dequeue()
    }
  }
  override def toString:String = "StringMatcherImpl"

}
