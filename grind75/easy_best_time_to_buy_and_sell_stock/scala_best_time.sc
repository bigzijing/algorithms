// This one is pretty straightforward
// To find max profit, you have to keep track of when price is the lowest, and the highest profit as you iterate through the stock prices
// On every iteration, see if current price - lowest price > current highest profit, and if current price is lower than tracked lowest price
def maxProfit(prices: Array[Int]): Int = {
  prices.foldLeft((-1, prices(0))) {
    case ((maxProfit, minPrice), currentPrice) =>
      (math.max(maxProfit, currentPrice - minPrice), math.min(minPrice, currentPrice))
  }._1
}

val testCase1 = (Array(7, 1, 5, 3, 6, 4), 5)
val testCase2 = (Array(7, 6, 4, 3, 1), 0)

def formatResults(testCase: (Array[Int], Int)) = {
  val (inputArray, expectedOutput) = testCase
  val result = maxProfit(inputArray)

  println(s"Input: prices = ${inputArray.mkString("[", ",", "]")}, target = $expectedOutput")
  println(s"Output: $result")
}

@main
val runTests = {
  formatResults(testCase1)
  formatResults(testCase2)
}