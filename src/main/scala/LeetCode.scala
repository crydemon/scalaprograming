object LeetCode {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    def go(s: Int, e: Int, arr: Array[(Int, Int)]): Array[Int] = {
      if (s >= e) {
        Array()
      } else if (arr(s)._1 + arr(e)._1 < target) {
        go(s + 1, e, arr)
      } else if (arr(s)._1 + arr(e)._1 > target) {
        go(s, e - 1, arr)
      } else {
        Array(math.min(arr(s)._2, arr(e)._2), math.max(arr(s)._2, arr(e)._2))
      }
    }

    val one = nums.zipWithIndex.sortBy(r => r._1)

    go(0, nums.length - 1, one)
  }

  def main(args: Array[String]): Unit = {
    println(twoSum(Array(2, 5, 5, 11), 10).toList)
  }
}
