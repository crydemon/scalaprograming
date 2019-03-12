package s99

import scala.annotation.tailrec

// Find the last element of a list.
object p1 extends App {
  @tailrec
  def last[A](l: List[A]): Option[A] = l match {
    case h :: Nil => Some(h)
    case _ :: t => last(t)
    case _ => None
  }


  def last1[A](l: List[A]): Option[A] = {
    @tailrec
    def go(h: A, t: List[A]): Option[A] = {
      if (h == Nil) None
      if (t == Nil) Some(h)
      else {
        val h = t.head
        val t1 = t.tail
        go(h, t1)
      }
    }

    if (l == Nil) {
      None
    } else {
      go(l.head, l.tail)
    }
  }

  def last2[A](l: List[A]): Option[A] = {
    if (l == Nil) None
    else Some(l(l.length - 1))
  }

  println(last(List(1, 3, 45, 6)))
  println(last1(List("i", "think", "scala", "gorgeous")))
  println(last2(Nil))
  println(last2(List(List(12, 3, 43, 54, 65), List(34, 3423), List(324, 34234, 32423423, 324))))
}

//Find the last but one element of a list.
object p2_p3 extends App {
  def lastNthBuiltin[A](n: Int, ls: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (ls.length < n) throw new NoSuchElementException
    ls.takeRight(n).head
  }

  //先让resultList移动n,
  //则resultList从n 到Nil移动 l.length
  //curList从0到l.length -n
  def lastNth1[A](n: Int, ls: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil => resultList.head
        case _ :: tail =>
          lastNthR(count - 1,
            if (count > 0) resultList else resultList.tail,
            tail)
      }

    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }

  println(lastNth1(3, List(1, 3, 45, 6, 3, 34, 32, 54)))
}

object p4 extends App {
  def length(xs: List[Int]): Int = {
    @tailrec
    def go(n: Int, xs: List[Int]): Int = {
      if (xs.isEmpty) n
      else {
        go(n + 1, xs.tail)
      }
    }

    go(0, xs)
  }

  println(length(List.fill(100000000)(10)))
}

object p5 extends App {
  def reverse(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case h :: t => reverse(t) ::: List(h)
    }
  }

  def reverse1[A](xs: List[A]): List[A] = {
    def go(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result
      case h :: t => go(h :: result, t)
    }

    go(Nil, xs)
  }


  def reverse2[A](xs: List[A]): List[A] = {
    xs.foldLeft(List[A]())((r, h) => h :: r)
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  def reverse3[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => h :: acc)

  val xs = List.fill(10)(scala.util.Random.nextInt(100))
  println(xs)
  println(reverse3(xs))
}

object p6 extends App {
  def isPalindrome[A](xs: List[A]): Boolean = {
    xs.foldLeft(List[A]())((acc, h) => h :: acc) == xs
  }

  val xs = List.fill(10)(scala.util.Random.nextInt(100))
  println(xs)
  println(isPalindrome(xs))
  println(isPalindrome(List(1, 2, 3, 2, 1)))
  println(isPalindrome(List(1, 2, 2, 1)))
}