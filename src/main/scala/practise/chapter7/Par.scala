package practise.chapter7

import java.util.concurrent._

// 构建出简单的可组合的核心数据类型和函数，才是重点
//赋予api代数性质
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

  /** Implement map3, map4, and map5 in terms of map2 */
  def map3[A, B, C, D](fa: Par[A], fb: Par[B], fc: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)(_ (_))
  }

  def map4[A, B, C, D, E](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => f(a, b, c, d)), fc)(_ (_)), fd)(_ (_))
  }

  def map5[A, B, C, D, E, F](fa: Par[A], fb: Par[B], fc: Par[C], fd: Par[D], fe: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    map2(map2(map2(map2(fa, fb)((a, b) => (c: C) => (d: D) => (e: E) => f(a, b, c, d, e)), fc)(_ (_)), fd)(_ (_)), fe)(_ (_))
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
      * theorem
      */

    iter(as)
  }

  /** Exercise 8
    *
    * Show why fork(x) == x does not hold for all ExecutorServices
    *
    * The implementation of fork queues the computation to be executed in the threadpool then blocks on that Future.
    * This is a problem, however, as the singleThreadedExecutorPool only has one thread, so this will result in a
    * non-terminating computation due to deadlock. (i.e., the single thread in the pool is blocked waiting for a
    * Callable that will never be dequeued, due to the thread being blocked)
    */

  /** Exercise 9
    *
    * Can you show that any sized threadpool can be made to deadlock given this implementation of fork?
    */
  //要么修复，要么声明有效的前提条件
  def deadlock[A](threadPoolSize: Int, a: A): Par[A] = {
    if (threadPoolSize <= 1) {
      async(a)
    } else {
      fork(deadlock(threadPoolSize - 1, a))
    }
  }

  def async[A](a: => A): Par[A] = fork(unit(a))

  /** Exercise 10
    *
    * Our non-blocking representation does not currently handle errors at all. How can you change the representation
    * to do so?
    *
    * See scala.concurrent.Future or scalaz Task ...
    *
    * The Future basically needs to wrap over a Try or an Either. Then, wrap the run of the Future in a try catch,
    * and if an exception is thrown propagate the Left or Failure result instead.
    */

  /** Exercise 11
    *
    * Implement choiceN and then choice in terms of it
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => choices(run(es)(n))(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond) {
    if (_) 1 else 0
  })(List(t, f))

  /** Exercise 12
    *
    * Implement choiceMap
    */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => choices(run(es)(key))(es)

  /** Exercise 13
    *
    * Implement chooser, then re-implement choice, choiceN and choiceMap in terms of it
    */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = { es => choices(run(es)(pa))(es) }

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond) {
    if (_) t else f
  }

  def choiceMap2[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = chooser(key)(choices)

  def run[A](es: ExecutorService)(a: Par[A]): A = a(es).get

  /** Exercise 14
    *
    * Implement join. Implement flatMap in terms of join. Implement join in terms of flatMap.
    */
  def join[A](a: Par[Par[A]]): Par[A] = { es => run(es)(a)(es) }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]) = join(map(a)(f))

  def join2[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

  def main(args: Array[String]): Unit = {
    println(map(unit(1))(_ + 1) == unit(2))
    // some point they are equal
    val executorService: ExecutorService = Executors.newCachedThreadPool()
    println(equal(executorService)(map(unit(1))(_ + 1), unit(2)))
  }

}
