package net.szumo.fstl

import scala.collection.mutable
import scala.annotation.tailrec

protected class Node[Output] {
  val number = Node.number
  Node.number += 1
  var outputs = Seq.empty[Output]
  var transitions = Map.empty[Char, Node[Output]]
  var fail: Node[Output] = null
  override def toString = s"Node $number"
  def longString = s"Node $number [$outputs] transitions $transitions fails to ${if (fail==null) "null" else fail}"
}

protected class Root[Output] extends Node[Output] {
}

protected object Node {
  var number = 1
  @tailrec
  def makeChildren[Output](node: Node[Output], s: String, caseType: CaseType):Node[Output] = {
    val newNode = node.transitions.getOrElse(s.head, {
      val n = new Node[Output]()
      node.transitions = caseType.addEquivalents(node.transitions, s.head, n)
      n
    })
    val tail = s.tail
    if (tail.isEmpty) newNode else makeChildren(newNode, tail, caseType)
  }
  @tailrec
  def find[Output](node:Node[Output], c:Char):Node[Output] = {
    val result = node.transitions.get(c)
    result match {
      case Some(x) => x
      case None => if (node.fail == null) node else find(node.fail, c)
    }
  }
  def apply[Output](words: Iterable[String], caseType: CaseType, resultFunc: String => Output):Node[Output] = {
    val root:Node[Output] = new Node[Output]()
    for (word <- words) {
      val child = Node.makeChildren(root, word, caseType)
      child.outputs = child.outputs ++ Seq(resultFunc(word))
    }
    // set suffixes
    val queue = mutable.Queue.empty[Node[Output]]
    root.transitions.values.foreach(node => node.fail = root)
    queue ++= root.transitions.values
    println(root.longString)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      println(current.longString)
      for ( (a, node) <- current.transitions) {
        queue.enqueue(node)
        var fail = current.fail
        while (fail.transitions.get(a) == None && fail != root) {
          fail = fail.fail
        }
        if (fail != root) {
          node.fail = fail.transitions(a)
          node.outputs = node.outputs ++ node.fail.outputs
        }
        else {
           node.fail = root
        }
      }
    }
    root
  }
}

protected class StringMatcherImpl[Output,Result](words: Iterable[String], caseType: CaseType, resultFunc: String => Output,
                                       createResult: (Output, Int) => Result) extends StringMatcher[Result] {
  def isMatch(s: String) = !(new State(Iterator(s)).isEmpty)
  def isMatch(s: Iterable[String]) = !(new State(s.iterator).isEmpty)
  def apply(s: String):Iterator[Result] = new State(Iterator(s))
  def apply(s: Iterable[String]): Iterator[Result] = new State(s.iterator)

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
        val c = chars.next()
        val next = Node.find(node,c)
        println(s"${node.longString} --- $c ---> $next")
        node = next
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
