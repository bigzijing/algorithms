/**
 * Definition for singly-linked list.
 */
class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

// Tail-recursive approach
// Note that the definition of ListNode isn't very idiomatic Scala so it is a bit unintuitive for me
// Also, a singly-linked list in Scala is best represented by Scala std lib's List which is optimized and has many goodies
// Or a case class would be a better choice as the class is expected to store values, and it would come with some of above mentioend goodies too (like deconstruction and pattern matching)
def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {

  // The ListNode class definition has a non FP approach where _x and _next has to be initialized, hence to do this in a FP tail-recursive way,
  // we'd have to initialize it -- which is the variable `uselessHead`
  // We have to iterate through the list so we have another variable `dummy` pointed to `uselessHead`
  val uselessHead = new ListNode()
  val dummy = uselessHead

  // If both lists are completely iterated, we call `.next` on `dummy` as we want to skip the useless initial values
  // Otherwise, if one of the lists is empty, we can stop iterating and comparing and append the non-empty list as next
  // Otherwise, we compare each `_x` value of each list, appending the smaller one to our accumulator and moving to the next element in the list
  @annotation.tailrec
  def loop(list1: ListNode, list2: ListNode, acc: ListNode): ListNode = (list1, list2) match {
    case (null, null) => dummy.next
    case (null, _) =>
      acc.next = list2
      dummy.next
    case (_, null) =>
      acc.next = list1
      dummy.next
    case _ => if (list1.x < list2.x) {
      acc.next = list1
      loop(list1.next, list2, acc.next)
    } else {
      acc.next = list2
      loop(list1, list2.next, acc.next)
    }
  }

  loop(list1, list2, dummy)
}

def convertTestCase(list1: List[Int], list2: List[Int], output: List[Int]): (Option[ListNode], Option[ListNode], Option[ListNode]) = {
  def loop(ls: List[Int]): Option[ListNode] = ls match {
    case Nil => None
    case last :: Nil => Some(new ListNode(last, null))
    case head :: tail => Some(new ListNode(head, loop(tail).orNull))
  }

  val ls1 = loop(list1)
  val ls2 = loop(list2)
  val op = loop(output)

  (ls1, ls2, op)
}

val testCase1 = (List(1, 2, 4), List(1, 3, 4), List(1, 1, 2, 3, 4, 4))
val testCase2 = (List.empty[Int], List.empty[Int], List.empty[Int])
val testCase3 = (List.empty[Int], List(0), List(0))

def formatListNode(listNodeOpt: Option[ListNode]): String = {
  def loop(acc: List[Int], ln: ListNode): List[Int] =
    if (ln == null) acc
    else loop(ln.x :: acc, ln.next)

  listNodeOpt match {
    case None => "[]"
    case Some(listNode) =>
      loop(List.empty[Int], listNode)
        .reverse
        .mkString("[", ",", "]")
  }
}

def formatResults(testCase: (List[Int], List[Int], List[Int]), approach: (ListNode, ListNode) => ListNode): Unit = {
  val (rawInput1, rawInput2, rawInputTarget) = testCase
  val (input1, input2, inputTarget) = convertTestCase(rawInput1, rawInput2, rawInputTarget)

  val result = approach(input1.orNull, input2.orNull)

  println(s"Input: list1 = ${rawInput1.mkString("[", ",", "]")}, list2 = ${rawInput2.mkString("[", ",", "]")}, target = ${rawInputTarget.mkString("[", ",", "]")}")
  println(s"Output: ${formatListNode(Some(result))}")
}

@main
val runTests = {
  val tailRecApproach = (list1: ListNode, list2: ListNode) => mergeTwoLists(list1, list2)

  formatResults(testCase1, tailRecApproach)
  formatResults(testCase2, tailRecApproach)
  formatResults(testCase3, tailRecApproach)
}