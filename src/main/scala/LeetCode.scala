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


  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      x + (if (next == null) "" else ", " + next.toString)
    }
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def go(l1: ListNode, l2: ListNode, carry: Int, result: ListNode): ListNode = {
      if (l1 == null && l2 == null && carry == 0) result
      else {
        val sum = ((if (l1 == null) 0 else l1.x) + (if (l2 == null) 0 else l2.x) + carry)
        result.x = sum % 10
        val carryNext: Int = sum / 10
        val (l1Next, l2Next) = (if (l1 == null) null else l1.next, if (l2 == null) null else l2.next)
        if (l1Next == null && l2Next == null && carryNext == 0) result
        else {
          result.next = new ListNode()
          go(l1Next, l2Next, carryNext, result.next)
        }
      }
    }

    val result = new ListNode()
    go(l1, l2, 0, result)
    result
  }

  def lengthOfLongestSubstring(s: String): Int = {
    def go(longest: Int, curStart: Int, curEnd: Int, s: String): Int = {
      if (curEnd == s.length) longest
      else {
        val ch = s.charAt(curEnd)
        s.substring(0, curEnd).indexOf(ch, curStart) match {
          case -1 => go(math.max(longest, curEnd + 1 - curStart), curStart, curEnd + 1, s)
          case x => go(longest, x + 1, curEnd + 1, s)
        }
      }
    }

    if (s.length <= 1) s.length
    else go(1, 0, 1, s)
  }

  def main(args: Array[String]): Unit = {
    println(lengthOfLongestSubstring("abcabcbb"))
    println(lengthOfLongestSubstring("bbtablud"))
    //    val l1 = new ListNode(2)
    //    l1.next = new ListNode(4)
    //    l1.next.next = new ListNode(3)
    //    val l2 = new ListNode(5)
    //    l2.next = new ListNode(6)
    //    l2.next.next = new ListNode(4)
    //    println(l1.toString)
    //    println(l2.toString)
    //    println(addTwoNumbers(l1, l2))
    //println(twoSum(Array(2, 5, 5, 11), 10).toList)
  }
}
