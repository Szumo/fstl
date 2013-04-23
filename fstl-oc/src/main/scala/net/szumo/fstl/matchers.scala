package net.szumo.fstl

import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.immutable.{TreeMap, WrappedString}

protected class StringMatcherImpl[Output](words: Iterable[String], caseType: CaseType, resultFunc: String => Output) extends StringMatcher[Output] {
  type Transitions = (Int) => Map[Char,Int]
  type Outputs = Int => Iterable[String]
  type Failures = Int => Int

  private def join(strings: Iterable[String]) = strings.flatMap(s => s.toIterator).toIterator
  def isMatch(s: String) = !(new State(s.toIterator).isEmpty)
  def isMatch(s: Iterable[String]) = !(new State(join(s)).isEmpty)
  def apply(s: String):Iterator[Output] = new State(s.toIterator)
  def apply(s: Iterable[String]): Iterator[Output] = new State(join(s))
  val (outputs, failures, transitions, size, root) = makeNodes(words, caseType)

  private def makeNodes(words: Iterable[String], caseType: CaseType): (Outputs, Failures, Transitions, Int, Int) = {
    var outputs = Map.empty[Int, Set[String]].withDefaultValue(Set.empty)
    val failures = mutable.ArrayBuffer(0)
    val transitions = mutable.ArrayBuffer[Map[Char,Int]](Map.empty)
    var lastNode: Int = 0
    val root = 0
    def makeNode() = {
      lastNode = lastNode + 1
      failures += -1
      transitions += Map.empty
      lastNode
    }
    @tailrec
    def makeChild(node: Int, s: WrappedString, caseType: CaseType):Int = {
      val head = s.head
      val tail = s.tail
      val result = transitions(node).getOrElse(head, {
        val newNode = makeNode()
        transitions(node) = transitions(node) ++ caseType.variants(head).map(c => (c,newNode))
        newNode
      })
      if (tail.isEmpty) result else makeChild(result, tail, caseType)
    }
    @tailrec
    def findExisting(node:Int, s: WrappedString):Int = {
      val tail = s.tail
      if (tail.isEmpty) node else {
        var current = Option(node)
        for (c <- tail) {
          current match {
            case Some(x) => current = transitions(x).get(c)
            case _ => ()
          }
        }
        current match {
          case Some(x) if x != node => x
          case _ => findExisting(node, tail)
        }
      }
    }
    words.foreach(word => {
      val child = makeChild(root, word, caseType)
      outputs = outputs + (child -> (outputs(child) ++ Seq(word)))
    })
    // set suffixes
    val queue = mutable.ArrayBuffer.empty[(String, Int)]
    transitions(root).values.foreach( t => failures(t) = root)
    queue ++= transitions(root).map( t => (t._1.toString, t._2))
    for ( (prefix, current) <- queue) {
      for ( (a, node) <- transitions(current)) {
        if (failures(node) == -1) {
          val s = prefix+a
          queue += s -> node
          val fail = findExisting(root, s)
          failures(node) = fail
          outputs = outputs + (node -> (outputs(node) ++ outputs(fail)))
        }
      }
    }
    val result = (outputs, failures.toArray.apply _, transitions , lastNode + 1, root)
    result
  }
  sealed class State(val chars: Iterator[Char]) extends Iterator[Output] {
    var lastFound = mutable.ArrayBuffer.empty[Output]
    var node = root
    def hasNext: Boolean = {
      findNext()
      lastFound.nonEmpty
    }
    def next(): Output = {
      findNext()
      lastFound.remove(0)
    }
    private def findNext() {
      if (lastFound.isEmpty && chars.hasNext) {
        var c = chars.next()
        var continue = true
        do {
          transitions(node).get(c) match {
            case Some(x) => {
              node = x
              if (outputs(node).nonEmpty) {
                continue = false
              } else {
                if (chars.hasNext) c = chars.next() else continue = false
              }
            }
            case None => if (failures(node) == node) {
              if (chars.hasNext) c = chars.next() else continue = false
            } else {
              node = failures(node)
            }
          }
        } while (continue)
        lastFound = lastFound ++ outputs(node).map(resultFunc)
      }
    }
  }
  override def toString:String = "StringMatcherImpl"
}
