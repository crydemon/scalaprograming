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

  def findMedianSortedArrays1(nums1: Array[Int], nums2: Array[Int]): Double = {
    def go(i1: Int, i2: Int, result: List[Int]): List[Int] = {
      if (i1 >= nums1.length) nums2.drop(i2).toList.reverse ::: result
      else if (i2 >= nums2.length) nums1.drop(i1).toList.reverse ::: result
      else if (nums1(i1) < nums2(i2)) go(i1 + 1, i2, nums1(i1) :: result)
      else go(i1, i2 + 1, nums2(i2) :: result)
    }

    val result = go(0, 0, List())
    val len = result.length
    if (len % 2 == 1) result(len / 2)
    else (result(len / 2) + result((len - 1) / 2)) * 1.0 / 2
  }

  //问题可以归结为在两个已排好序的数组中找第k大的数
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    def findKthNumber(l1: List[Int], l2: List[Int], k: Int): Int = {
      if (l1.length > l2.length) findKthNumber(l2, l1, k)
      else if (l1 == Nil) l2(k - 1)
      else if (k == 1) math.min(l1(0), l2(0))
      else {
        val a = math.min(k / 2, l1.length)
        val b = k - a
        //已找到b个最小
        if (l1(a - 1) > l2(b - 1)) findKthNumber(l2.drop(b), l1, k - b)
        //已找到a个最小
        else if (l1(a - 1) < l2(b - 1)) findKthNumber(l1.drop(a), l2, k - a)
        else l1(a - 1)
      }
    }

    val len = nums1.length + nums2.length
    val l1 = nums1.toList
    val l2 = nums2.toList
    if (len % 2 == 1) findKthNumber(l1, l2, (len + 1) / 2)
    else (findKthNumber(l1, l2, len / 2) + findKthNumber(l1, l2, len / 2 + 1)) * 1.0 / 2
  }

  def longestPalindrome(s: String): String = {
    def isPalindrome(l1: Int, r1: Int, l2: Int, r2: Int): (Int, Int) = {
      var l = l1
      var r = r1
      while (l > 0 && r < s.length - 1 && s.charAt(l - 1) == s.charAt(r + 1)) {
        l = l - 1
        r = r + 1
      }
      if (r2 - l2 < r - l) (l, r)
      else (l2, r2)
    }

    var result = (0, 0)
    for (i <- 0 to s.length - 2) {
      var l = i
      var r = i
      // i为回文中心， x为i左边， y为i右边 的连续重复字符数
      //如果 x + y = odd
      //  1. 如果x = y 显然 longest = x + x  + isPalindrome(i-x, i +x) + 1
      //  2. 如果x < y 显然 以此位置为中心的回文不大于 x + x + 1,因为i-x-1 位置不等于i+x+1
      //如果 x + y = even
      //  1. 如果x = y 显然 longest = x + x  + isPalindrome(i-x, i +x)
      //  2. 如果x < y 显然 以此位置为中心的回文不大于 x + x
      //所以 当遇到连续重复字符时只需要求
      while (l > 0 && s.charAt(l - 1) == s.charAt(i)) {
        l = l - 1
      }
      while (r < s.length - 1 && s.charAt(r + 1) == s.charAt(i)) {
        r = r + 1
      }
      result = isPalindrome(l, r, result._1, result._2)
    }
    if (s.isEmpty) s
    else s.substring(result._1, result._2 + 1)
  }

  def main(args: Array[String]): Unit = {
    println(longestPalindrome("aaaa"))
    //    println(findMedianSortedArrays(Array(1, 2), Array(3, 4)))
    //    println(lengthOfLongestSubstring("abcabcbb"))
    //    println(lengthOfLongestSubstring("bbtablud"))
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
