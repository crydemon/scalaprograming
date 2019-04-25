class Parent {
  def foo(bar: Int = 1, baz: Int = 2): Int = {
    println("parent")
    println((bar, baz))
    bar + baz
  }
}

class Child extends Parent {
  override def foo(baz: Int = 3, bar: Int = 4): Int = {
    println("child")
    println((bar, baz))
    super.foo(baz, bar)
  }
}

object testPoly extends App {
  val x = new Child()
  val y: Parent = new Child
  println(x.foo())
  println(x.foo(bar = 1))
  println(y.foo())
  //名字是静态绑定的，值是动态绑定的。
  //编译器不会为在子类里重命名参数的行为给出警告信息，所以我们需要注意变量的取名。
  println(y.foo( 545, 1))
}