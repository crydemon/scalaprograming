import java.util

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

  //0 1 2 3(+1) 2 1 0(-1)
  def convert(s: String, numRows: Int): String = {
    if (numRows == 1 || numRows >= s.length) s
    else {
      val result = new Array[String](numRows).map(_ => "")
      var index = 0
      var step = 1
      for (c <- s) {
        result(index) += c
        if (index == numRows - 1) {
          step = -1
        }
        else if (index == 0) {
          step = 1
        }
        index = index + step
      }
      result.foldLeft("")(_ + _)
    }

  }

  def reverse(x: Int): Int = {
    val s = Math.abs(x).toString
    try {
      val reverse = s.reverse.toInt
      if (x > 0) reverse
      else -1 * reverse
    }
    catch {
      case _: Exception => 0
    }
  }

  def myAtoi(str: String): Int = {
    try {
      val t = str.trim
      val s = if (t.charAt(0) == '+' || t.charAt(0) == '-') t.substring(1) else t
      val flag = if (t.charAt(0) == '-') -1 else 1
      val result = s.takeWhile(c => c >= '0' && c <= '9').foldLeft(0l)((acc, ch) => {
        val cur = acc * 10 + (ch - '0') * flag
        if (cur <= Int.MinValue) Int.MinValue
        else if (cur >= Int.MaxValue) Int.MaxValue
        else cur
      })
      result.toInt
    }
    catch {
      case _: Exception => 0
    }
  }

  def isMatch(s: String, p: String): Boolean = {
    if (p.isEmpty) s.isEmpty
    else if (s.length > 0 && (s.head == p.head || p.head == '.')) {
      if (p.length > 1 && p.tail.head == '*') isMatch(s.tail, p) || isMatch(s, p.tail.tail)
      else isMatch(s.tail, p.tail)
    }
    else if (p.length > 1 && p.tail.head == '*') isMatch(s, p.tail.tail)
    else false
  }


  def isPalindrome(x: Int): Boolean = {
    val s = x.toString
    s.reverse == s
  }

  def maxArea(height: Array[Int]): Int = {
    //可以算出当前短板的最大容量
    // 因为起始时，长最大，所以只需找出i,j中的最小值，便可算得当前短板的最大容量
    //然后抛弃这个短板，在剩下的（i-1, j）或（i, j-1）中找到此刻短板的最大容量，跟（i,j）短板的最大容量比较
    //重复上述步骤，便可获得最大容量
    def go(i: Int, j: Int, result: Int): Int = {
      if (i >= j) result
      else if (height(i) > height(j)) go(i, j - 1, math.max(result, (j - i) * height(j)))
      else go(i + 1, j, math.max(result, (j - i) * height(i)))
    }

    go(0, height.length - 1, 0)
  }

  //除特殊表示之外，其余的都是重复当前的数字表示+1
  def intToRoman(num: Int): String = {
    val nums = Array(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    val Roman = Array("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")

    def go(a: Int, i: Int, result: List[String]): List[String] = {
      if (a == 0) result
      else go(a % nums(i), i + 1, result ++ List.fill(a / nums(i))(Roman(i)))
    }

    go(num, 0, Nil).foldLeft("")(_ + _)
  }

  def romanToInt(s: String): Int = {
    val map = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

    def go(i: Int, result: Int): Int = {
      if (i >= s.length) result
      else if (i + 1 < s.length && map.getOrElse(s(i), 0) < map.getOrElse(s(i + 1), 0)) {
        go(i + 1, result - map.getOrElse(s(i), 0))
      } else {
        go(i + 1, result + map.getOrElse(s(i), 0))
      }
    }

    go(0, 0)
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.isEmpty) ""
    else strs.head.zipWithIndex.takeWhile(r => strs.forall(s => r._2 < s.length && s(r._2) == r._1)).map(_._1).mkString
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val sorted = nums.sorted
    var result = List[List[Int]]()
    for (x <- 0 until sorted.length
         if sorted(x) <= 0
         if x == 0 || sorted(x) != sorted(x - 1)
    ) {
      var (l, r) = (x + 1, sorted.length - 1)
      while (l < r) {
        sorted(l) + sorted(x) + sorted(r) match {
          case sum if sum > 0 => r -= 1
          case sum if sum < 0 => l += 1
          case 0 => {
            result = result :+ List(sorted(l), sorted(x), sorted(r))
            do l += 1 while (l < r && sorted(l) == sorted(l - 1))
            do r -= 1 while (l < r && sorted(r) == sorted(r + 1))
          }
        }
      }
    }
    result
  }


  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    val sorted = nums.sorted
    var result = nums.take(3).sum
    for (x <- 0 until sorted.length
         if result != target
    ) {
      var (l, r) = (x + 1, sorted.length - 1)
      while (l < r) {
        val sum = sorted(l) + sorted(x) + sorted(r)
        if (math.abs(sum - target) < math.abs(result - target)) result = sum
        if (sum < target) l += 1
        else r -= 1
      }
    }
    result
  }

  def grayCode(n: Int): List[Int] = {
    def go(len: Int, i: Int, result: List[Int]): List[Int] = {
      if (i == len) result
      else go(len, i + 1, (i ^ (i >> 1)) :: result)
    }

    go(math.pow(2, n).toInt, 0, Nil).reverse
  }


  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null

    override def toString: String = {
      value + "," + left.toString + "," + right.toString
    }
  }

  def pathSum(root: TreeNode, sum: Int): List[List[Int]] = {

    def go(node: TreeNode, n: Int, one: List[Int]): List[List[Int]] = {
      if (n == 0 && node == null) List(one.reverse)
      else if (node == null || node._value == one) Nil
      else if (node.left == null) go(node.right, n - node._value, node._value :: one)
      else if (node.right == null) go(node.left, n - node._value, node._value :: one)
      else go(node.left, n - node._value, node._value :: one) ::: go(node.right, n - node._value, node._value :: one)
    }

    if (root == null) Nil
    else go(root, sum, Nil)
  }

  def recoverTree(root: TreeNode): Unit = {
    var pre: TreeNode = null
    var first: TreeNode = null
    var second: TreeNode = null

    def go(node: TreeNode): Unit = {
      if (node == null) Unit
      else {
        go(node.left)
        if (pre != null && pre.value > node.value) {
          if (first == null) first = pre
          if (first != null) second = node
        }
        pre = node
        go(node.right)
      }
    }

    go(root)
    val tmp = first.value
    first.value = second.value
    second.value = tmp
  }

  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {

    def go(l2r: Boolean, queue: List[TreeNode], result: List[List[Int]]): List[List[Int]] = {
      if (queue == Nil) result
      else {
        val layer = queue.foldRight(List[TreeNode]())((n, acc) => (n.left, n.right) match {
          case (null, null) => acc
          case (l, null) => l :: acc
          case (null, r) => r :: acc
          case (l, r) => l :: r :: acc
        })
        if (l2r) go(false, layer, queue.map(_.value) :: result)
        else go(true, layer, queue.map(_.value).reverse :: result)
      }
    }

    if(root == null) Nil
    else go(true, List(root), Nil).reverse
  }

//  def sumNumbers(root: TreeNode): Int = {
//
//  }

  //scala为什么运行那么慢
  def main(args: Array[String]): Unit = {
    val root = new TreeNode(3)
    root.left = new TreeNode(1)
    root.right = new TreeNode(4)
    root.right.left = new TreeNode(2)
    println(zigzagLevelOrder(root))
    //recoverTree(root)
    //    val root = new TreeNode(5)
    //    root.left = new TreeNode(4)
    //    root.left.left = new TreeNode(11)
    //    root.left.left.left = new TreeNode(7)
    //    root.left.left.right = new TreeNode(2)
    //
    //    root.right = new TreeNode(8)
    //    root.right.left = new TreeNode(13)
    //    root.right.right = new TreeNode(4)
    //    root.right.right.left = new TreeNode(5)
    //    root.right.right.right = new TreeNode(1)
    //    println(pathSum(root, 22))
    //println(grayCode(2))
    //println(threeSumClosest(Array(0, 2, 1, -3), 1))
    //println(threeSum(Array(-2, 0, 0, 2, 2)))
    //println(longestCommonPrefix(Array("flower", "flow", "flight")))
    //    println(romanToInt("MCMXCIV"))
    //    println(intToRoman(58))c
    //println(maxArea(Array(1, 3, 2, 5, 25, 24, 5)))
    //    println(isMatch("aa", ".*c"))
    //    println(isPalindrome(131))
    //println(myAtoi("4193 with words"))
    //9646324351
    //2147483647
    //2147483647
    //println(reverse(2147483647))
    //println(convert("PAYPALISHIRING", 4))
    //println(longestPalindrome("aaaa"))
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
