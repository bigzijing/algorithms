// Intuitive but not idiomatic functional approach
def isPalindrome(s: String): Boolean = {
  val cleanedString = s
    .replaceAll("[^A-Za-z0-9]", "") // removes all whitespace and non alphanumeric characters
    .map(_.toLower) // maps string to lower since case does not matter

  val strLen = cleanedString.length

  val middleIndex = if (strLen % 2 == 1) strLen / 2 // Int division in Scala floors non whole numbers, so if the String has an odd number length, this gives us the middle index
    else (strLen / 2) - 1 // if the String has an even number length, we minus one to account for index 0

  val reversedStr = cleanedString.reverse // we iterate the cleaned string and the reverse of it up to the middle point and check for equality

  for (i <- 0 to middleIndex)
    if (cleanedString(i) != reversedStr(i)) return false

  true
}

// Tail-recursive and complete approach
def isPalindromeTailRec(s: String): Boolean = {
  val cleanedString = s
    .replaceAll("[^A-Za-z0-9]", "") // removes all whitespace and non alphanumeric characters
    .map(_.toLower) // maps string to lower since case does not matter

  @annotation.tailrec
  def loop(i: Int, j: Int): Boolean = {
    if (j < i) true // recursion stops when the pointer iterating from the back reaches a lower value than the pointer iterating from the front
    else if (cleanedString(i) != cleanedString(j)) false // equality check
    else loop(i + 1, j - 1)
  }

  loop(0, cleanedString.length - 1)
}

val testCase1 = ("A man, a plan, a canal: Panama", true)
val testCase2 = ("race a car", false)
val testCase3 = (" ", true)

def formatResults(testCase: (String, Boolean), approach: String => Boolean): Unit = {
  val (inputString, inputTarget) = testCase

  val result = approach(inputString)

  println(s"Input: s = \"$inputString\", target = $inputTarget")
  println(s"Ouput: $result")
}

@main
val runTests = {
  val imperativeApproach = (s: String) => isPalindrome(s)
  val tailRecApproach = (s: String) => isPalindromeTailRec(s)

  formatResults(testCase1, imperativeApproach)
  formatResults(testCase2, imperativeApproach)
  formatResults(testCase3, imperativeApproach)

  println("--------------------")
  println("Results from imperative approach:")
  formatResults(testCase1, imperativeApproach)
  formatResults(testCase2, imperativeApproach)
  formatResults(testCase3, imperativeApproach)

  println("--------------------")
  println("Results from imperative while approach:")
  formatResults(testCase1, tailRecApproach)
  formatResults(testCase2, tailRecApproach)
  formatResults(testCase3, tailRecApproach)
}