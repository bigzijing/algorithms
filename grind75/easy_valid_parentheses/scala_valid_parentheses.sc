import scala.collection.mutable

// Tail-recursive approach
// We first convert the string into a List to make use of syntactic sugar to iterate over each Char of the string
// We keep an accumulator for the last opening bracket that we accumulated to compare it with incoming char
def isValid(s: String): Boolean = {
  val sLs = s.toList

  // if the next Char in the string is an opening bracket, it will always be a correct syntax
  val openers = List('(', '[', '{')
  def isOpening(char: Char) = openers.contains(char)

  // checks if incoming bracket matches last opening bracket
  def matchesOpening(opening: Char, closing: Char): Boolean = (opening, closing) match {
    case ('(', ')') | ('[', ']') | ('{', '}') => true
    case _ => false
  }

  @annotation.tailrec
  // reverseAcc is an accumulator in reverse since we want FILO property, like a Stack, and list prepend performs better
  def loop(charList: List[Char], reverseAcc: List[Char] = List.empty[Char]): Boolean = charList match {
    // we finished iterating the string, so we check finally if accumulator is empty (no unclosed brackets)
    case Nil => reverseAcc.isEmpty
    // incoming char is an opening bracket, we prepend it to accumulator
    case head :: tail if isOpening(head) => loop(tail, head :: reverseAcc)
    case head :: tail => reverseAcc match {
      // we have no accumulated opening bracket but incoming char is closing bracket, so return false
      case Nil => false
      // if incoming closing bracket matches the last opening bracket accumulated, we move to next iteration, else false
      case latestOpening :: accTail => if (matchesOpening(latestOpening, head)) loop(tail, accTail) else false
    }
  }

  loop(sLs)
}

// Imperative approach using a mutable Stack
def isValidStack(s: String): Boolean = {
  import scala.collection.mutable.Stack

  // if the next Char in the string is an opening bracket, it will always be a correct syntax
  val openers = List('(', '[', '{')
  def isOpening(char: Char) = openers.contains(char)

  // checks if incoming bracket matches last opening bracket
  def matchesOpening(opening: Char, closing: Char): Boolean = (opening, closing) match {
    case ('(', ')') | ('[', ']') | ('{', '}') => true
    case _ => false
  }

  val stack = mutable.Stack.empty[Char]

  for (char <- s) {
    if (isOpening(char)) stack.push(char)
    else {
      if (stack.isEmpty) return false
      else {
        if (!matchesOpening(stack.pop, char)) return false
      }
    }
  }

  stack.isEmpty
}

val testCase1 = "()"
val testCase2 = "()[]{}"
val testCase3 = "(]"

def formatResults(testCase: String, approach: String => Boolean): Unit = {
  val result = approach(testCase)

  println(s"Input: s = \"$testCase\"")
  println(s"Output: $result")
}

@main
val runTests = {
  val tailRecApproach = (s: String) => isValid(s)
  val imperativeApproach = (s: String) => isValidStack(s)

  formatResults(testCase1, tailRecApproach)
  formatResults(testCase2, tailRecApproach)
  formatResults(testCase3, tailRecApproach)

  println("--------------------")
  println("Results from imperative approach:")
  formatResults(testCase1, imperativeApproach)
  formatResults(testCase2, imperativeApproach)
  formatResults(testCase3, imperativeApproach)
}