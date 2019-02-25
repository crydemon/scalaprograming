package practise.chapter7

import java.util.concurrent._

// 构建出简单的可组合的核心数据类型和函数，才是重点
object Par {
  type Par[A] = ExecutorService => Future[A]

  def sum(ints: IndexedSeq[Int]): Int =
    ints.foldLeft(0)(_ + _)

  def sum1(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l) + sum(r)
    }

  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    // convenience method on `List` for concatenating a list of lists
    map(sequence(pars))(_.flatten)
  }


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures.
    // This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
    // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them,
    // and wraps them in a `UnitFuture`.
    // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`,
    // then subtracts that time from the available time allocated for evaluating `bf`.

    UnitFuture(f(af.get, bf.get))
  }

  // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism. Essentially,
  // we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  /** Exercise 3
    *
    * Fix the implementation of map2 so as to respect timeouts
    */
  def map2b[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { (es: ExecutorService) =>
    val af = a(es)
    val bf = b(es)

    new Future[C] {
      def cancel(mayInterruptIfRunning: Boolean): Boolean = true

      def isCancelled: Boolean = af.isCancelled || bf.isCancelled

      def isDone: Boolean = af.isDone && bf.isDone

      def get(): C = f(af.get, bf.get)

      def get(timeout: Long, unit: TimeUnit): C = {
        val started = System.currentTimeMillis

        val a = af.get(timeout, unit)
        val elapsed = System.currentTimeMillis - started
        val remaining = unit.toMillis(timeout) - elapsed
        val b = bf.get(remaining, unit)

        f(a, b)
      }
    }
  }

  // TODO below two functions are ugly and stupid, fix them
  def reduce[A](as: IndexedSeq[A], zero: A)(f: (A, A) => A): Par[A] = {
    def iter(as: IndexedSeq[A]): Par[A] = {
      if (as.isEmpty) unit(zero)
      else {
        val left = fork(iter(as.take(as.length / 2)))
        val right = fork(iter(as.drop(as.length / 2)))

        map2(left, right)(f)
      }
    }

    /** Exercise 7
      *
      * Given map(y)(id) == y it is a free theorem that map(map(y)(g))(f) == map(y)(f compose g). Prove it.
      *
      * My knowledge of mathematical proof techniques is poor so this might be wrong.
      *
      * map(y)(f) == map(y)(f)                      -- trivially true
      * map(map(y)(id))(f) == map(y)(f)             -- as map(y)(id) == y
      * map(map(y)(id))(f) == map(y)(f compose id)  -- as f compose id == f
      * map(map(y)(g))(f) == map(y)(f compose g)    -- as id can be swapped with an arbitrary g because of the free
      *                                                theorem
      */

    iter(as)
  }


  def main(args: Array[String]): Unit = {
    println(map(unit(1))(_ + 1) == unit(2))// some point they are equal
    val executorService: ExecutorService = Executors.newCachedThreadPool()
    println(equal(executorService)(map(unit(1))(_ + 1), unit(2)))
  }

}
