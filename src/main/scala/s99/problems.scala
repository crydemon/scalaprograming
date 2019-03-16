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


object p7 extends App {
  //  @tailrec
  //  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  //    case Nil => z
  //    case h :: t => foldLeft(t, f(z, h))(f)
  //  }
  //
  //  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
  //    foldLeft(reverse(l), z)((b, a) => f(a, b))
  //
  //  def reverse[A](l: List[A]): List[A] =
  //    foldLeft(l, List[A]())((acc, h) => h :: acc)
  //
  //  def append[A](a1: List[A], a2: List[A]): List[A] =
  //    a1 match {
  //      case Nil => a2
  //      case h :: t => h :: append(t, a2)
  //    }
  //
  //  def concat[A](l: List[List[A]]): List[A] =
  //    foldRightViaFoldLeft(l, Nil: List[A])(append)
  //
  //  def map[A, B](l: List[A])(f: A => B): List[B] =
  //    foldRightViaFoldLeft(l, Nil: List[B])((h, t) => f(h) :: t)
  //
  //  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  //    concat(map(as)(f))

  def flatten(xs: List[_]): List[_] = xs flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
}

//删除连续重复元素
object p8 extends App {
  def compress[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: tail => h :: compress(tail.dropWhile(_ == h))
  }

  //尾递归，加一个保存当前状态的参数，
  def compress1[A](xs: List[A]): List[A] = {
    def go(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => go(h :: result, tail.dropWhile(_ == h))
      case Nil => result.reverse
    }

    go(Nil, xs)
  }

  def compress2[A](xs: List[A]): List[A] = {
    xs.foldRight(List[A]()) { (h, acc) =>
      if (acc.isEmpty || acc.head != h) h :: acc
      else acc
    }
  }

  val xs = List.fill(10)(scala.util.Random.nextInt(4))
  println(xs)
  println(compress(xs))
  println(compress1(xs))
  println(compress2(xs))
}

object p9 extends App {
  def pack[A](xs: List[A]): List[List[A]] = {
    if (xs.isEmpty) List(List())
    else {
      //span(f):(List[A], List[A])
      //将满足f的xs.head添加到list中
      val (packed, next) = xs span (_ == xs.head)
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def pack1[A](xs: List[A]): List[List[A]] = {
    def go(result: List[List[A]], curList: List[A]): List[List[A]] = curList match {
      case Nil => result.reverse
      case h :: _ => {
        val (h1, t1) = (curList span (_ == h))
        go(h1 :: result, t1)
      }
    }

    go(Nil, xs)
  }

  def encode[A](xs: List[A]): List[(Int, A)] = {
    pack1(xs).map(l => (l.length, l.head))
  }

  def encodeModified1[A](xs: List[A]): List[Any] = {
    encode(xs).map(l => if (l._1 == 1) l._2 else l)
  }

  def encodeModified2[A](xs: List[A]): List[Either[A, (Int, A)]] = {
    pack1(xs).map(l => if (l.length == 1) Left(l.head) else Right(l.length, l.head))
  }

  val xs = List.fill(20)(scala.util.Random.nextInt(3))
  println(xs)
  println(pack(xs))
  println(pack1(xs))
  println(encode(xs))
  println(encodeModified1(xs))
  println(encodeModified2(xs))
}

object p12 extends App {
  def make[A](length: Int, a: A): List[A] = {
    if (length > 0) a :: make(length - 1, a)
    else Nil
  }

  def make1[A](length: Int, a: A): List[A] = {
    var result: List[A] = List[A]()
    for (_ <- 0 to length) {
      result = a :: result
    }
    result
  }

  def decode[A](xs: List[(Int, A)]): List[A] = {
    xs.flatMap(e => make1
    (e._1, e._2))
  }

  val l = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  print(l)
}

object p13 extends App {
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case h :: _ => {
      val (h1, t1) = xs span (_ == h)
      (h1.length, h) :: encodeDirect(t1)
    }
  }

  def encodeDirect1[A](xs: List[A]): List[(Int, A)] = {

    def go(result: List[(Int, A)], curList: List[A]): List[(Int, A)] = curList match {
      case Nil => result.reverse
      case h :: t => {
        val (h1, t1) = t span (_ == h)
        println((h1, t1))
        val h2 = (h1.length + 1, h)
        go(h2 :: result, t1)
      }
    }

    go(Nil, xs)
  }

  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(encodeDirect1(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object p14 extends App {
  def duplicate[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: t => h :: h :: duplicate(t)
  }

  def duplicate1[A](xs: List[A]): List[A] = {
    def go(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result.reverse
      case h :: t => go(h :: h :: result, t)
    }

    go(Nil, xs)
  }

  def duplicate2[A](xs: List[A]): List[A] = {
    xs flatMap (e => List(e, e))
  }

  println(duplicate(List('a, 'b, 'c, 'c, 'd)))
  println(duplicate1(List('a, 'b, 'c, 'c, 'd)))
}

object p15 extends App {

  def duplicateN[A](n: Int, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: t => List.fill(n)(h) ++ duplicateN(n, t)
  }

  def duplicateN1[A](n: Int, xs: List[A]): List[A] = {
    def go(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result.reverse
      case h :: t => {
        go(List.fill(n)(h) ++ result, t)
      }
    }

    go(Nil, xs)
  }

  def duplicateN2[A](n: Int, xs: List[A]): List[A] = {
    xs.flatMap(e => List.fill(n)(e))
  }

  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
  println(duplicateN1(3, List('a, 'b, 'c, 'c, 'd)))
  println(duplicateN2(3, List('a, 'b, 'c, 'c, 'd)))
}

object p16 extends App {
  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(c - 1, tail)
    }

    dropR(n, ls)
  }

  def drop1[A](n: Int, xs: List[A]): List[A] = {
    def go(result: List[A], c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil) => result.reverse
      case (1, _ :: t) => go(result, n, t)
      case (_, h :: t) => go(h :: result, c - 1, t)
    }

    go(Nil, n, xs)
  }

  def drop2[A](n: Int, xs: List[A]): List[A] = {
    xs.zipWithIndex.filter(v => (v._2 + 1) % n != 0).map(v => v._1)
  }

  println(dropRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(drop1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(drop2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object p17 extends App {
  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = (n, xs) match {
    case (_, Nil) => (Nil, Nil)
    case (0, l) => (Nil, l)
    case (n, h :: t) => {
      val (pre, post) = split(n - 1, t)
      println((pre, post))
      (h :: pre, post)
    }
  }

  def split1[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    def go(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) = (curN, curL) match {
      case (_, Nil) => (pre.reverse, Nil)
      case (0, l) => (pre.reverse, l)
      case (n, h :: t) => go(n - 1, t, h :: pre)
    }

    go(n, xs, Nil)
  }

  def split2[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    (xs.take(n), xs.drop(n))
  }

  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(split1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(split2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object p18 extends App {
  def slice[A](s: Int, e: Int, xs: List[A]): List[A] = {
    xs.take(e).drop(s)
  }

  def slice1[A](s: Int, e: Int, xs: List[A]): List[A] = (s, e, xs) match {
    case (_, _, Nil) => Nil
    case (_, e, _) if e <= 0 => Nil //终止条件
    case (s, e, h :: t) if s <= 0 => h :: slice1(0, e - 1, t) //开始条件
    case (s, e, _ :: t) => slice1(s - 1, e - 1, t)
  }

  // Since several of the patterns are similar, we can condense the tail recursive
  // solution a little.
  def sliceTailRecursive2[A](start: Int, end: Int, ls: List[A]): List[A] = {
    def sliceR(count: Int, curList: List[A], result: List[A]): List[A] = {
      if (curList.isEmpty || count >= end) result.reverse
      else sliceR(count + 1, curList.tail,
        if (count >= start) curList.head :: result
        else result)
    }
    sliceR(0, ls, Nil)
  }

  println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  println(slice1(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}