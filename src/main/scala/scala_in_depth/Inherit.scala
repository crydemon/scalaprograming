package scala_in_depth




object TestInherit extends App {

  //从右往左，选择离G最远的trait，将该trait从自身到父类从左往右放置
  //将此时线性化的结果中重复值去掉，原则是多个元素保留最右
  //在最右加入AnyRef和Any，完成构建
  //G B A
  //G B A F C A
  //G B A F C A E C A
  //G B A F C A E C A D A
  //G B F C E C D A
  //G B F E C D A
  //G B F E C D A AnyRef Any
  class A {
    def m(s: String) = println(s"A($s)")
  }

  trait B extends A {
    override def m(s: String) = super.m(s"B($s)")
  }

  trait C extends A {
    override def m(s: String) = super.m(s"C($s)")
  }

  trait D extends A {
    override def m(s: String) = super.m(s"D($s)")
  }

  trait E extends C {
    override def m(s: String) = super.m(s"E($s)")
  }

  trait F extends C {
    override def m(s: String) = super.m(s"F($s)")
  }

  class G extends D with E with F with B {
    override def m(s: String) = super.m(s"G($s)")
  }

  val x = new G
  x.m("")
}