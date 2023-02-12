// Tail-recursive approach
def twoSum(nums: Array[Int], target: Int): Array[Int] = {

  // modular function to calculate the next indices to iterate over
  def getNextIndices(a: Int, b: Int): (Int, Int) =
    if (b + 1 == nums.length) (a + 1, a + 2)
    else (a, b + 1)

  @annotation.tailrec
  // The question states that there is *exactly one* solution, so we can return the results once we find it
  def loop(a: Int, b: Int, acc: Array[Int] = Array.empty[Int]): Array[Int] = {
    if (nums(a) + nums(b) == target) Array(a, b)
      // for an array of size n, we reach the end of iterations when (b == (n - 1) && a == (n - 2)) since arrays are 0-indexed
    else if (b + 1 == nums.length && a + 2 == nums.length) acc
    else {
      val (nextA, nextB) = getNextIndices(a, b)
      loop(nextA, nextB, acc)
    }
  }

  loop(0, 1)
}

// Imperative approach
// The imperative approach is to iterate nums with 2 for-loops, then returning when the indices are found
def twoSumImperative(nums: Array[Int], target: Int): Array[Int] = {
  for (i <- nums.indices) {
    for (j <- i + 1 until nums.length) {
      if (nums(i) + nums(j) == target) {
        return Array(i, j)
      }
    }
  }

  Array(-1, -1)
}

// Imperative while loop
def twoSumImperativeWhile(nums: Array[Int], target: Int): Array[Int] = {
  var i = 0

  while (i <= nums.length - 2) {
    for (j <- i + 1 until nums.length) {
      if (nums(i) + nums(j) == target) return Array(i, j)
    }
    i = i + 1
  }

  Array(-1, -1)
}

val testCase1 = (Array(2, 7, 11, 15), 9)
val testCase2 = (Array(3, 2, 4), 6)
val testCase3 = (Array(3, 3), 6)

def formatResults(testCase: (Array[Int], Int), approach: (Array[Int], Int) => Array[Int]): Unit = {
  val (inputArray, inputTarget) = testCase

  val result = approach(inputArray, inputTarget)
  val (a, b) = (result(0), result(1))

  println(s"Input: nums = ${inputArray.mkString("[", ",", "]")}, target = $inputTarget")
  println(s"Because nums[$a] + nums[$b] == $inputTarget, we return [$a, $b]\n")
}

@main
val runTests = {
  val recursiveApproach = (nums: Array[Int], target: Int) => twoSum(nums, target)
  val imperativeApproach = (nums: Array[Int], target: Int) => twoSumImperative(nums, target)
  val imperativeWhileApproach = (nums: Array[Int], target: Int) => twoSumImperativeWhile(nums, target)

  formatResults(testCase1, recursiveApproach)
  formatResults(testCase2, recursiveApproach)
  formatResults(testCase3, recursiveApproach)

  println("--------------------")
  println("Results from imperative approach:")
  formatResults(testCase1, imperativeApproach)
  formatResults(testCase2, imperativeApproach)
  formatResults(testCase3, imperativeApproach)

  println("--------------------")
  println("Results from imperative while approach:")
  formatResults(testCase1, imperativeWhileApproach)
  formatResults(testCase2, imperativeWhileApproach)
  formatResults(testCase3, imperativeWhileApproach)
}