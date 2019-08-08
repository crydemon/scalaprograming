class Vec2d(var x: Double, var y: Double) {
  def magnify(amt: Double): Vec2d = {
    y *= amt
    x *= amt
    this
  }

  //dont do this
  //  def -(another: Vec2d): Vec2d = {
  //    this.x -= another.x
  //    this.y -= another.y
  //    this
  //  }
  def -(another: Vec2d): Vec2d = {
    new Vec2d(x - another.x, y - another.y)
  }

  override def toString: String = {
    this.x + "," + this.y
  }
}

object Vec2d extends App {
  val x = new Vec2d(1, 1)
  val y = new Vec2d(-1, 1)
  println(x.magnify(3) - (x - y).magnify(3))
}


trait A {
  val repr: Int

  override def equals(obj: Any): Boolean = obj match {
    case obj: A => {
      if (this eq obj) true
      else {
        (obj.## == this.##) && (repr == obj.repr)
      }
    }
    case _ => false
  }

  override def hashCode(): Int = repr.##
}

trait B extends A {
  val name: String

  override def equals(obj: Any): Boolean = obj match {
    case obj: B => {
      if (this eq obj) true
      else {
        (repr == obj.repr) && (name == obj.name)
      }
    }
    case _ => false
  }
}

object test1 extends App {
  val x = new A {
    override val repr: Int = 2
  }
  val y = new B {
    override val repr: Int = 2
    override val name: String = "kkk"
  }
  println(x.getClass)
  println(y.getClass)
  println(x == y)
  println(y == x)
}


trait C extends Equals {
  val repr: Int

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[C]

  override def equals(obj: Any): Boolean = obj match {
    case obj: C => {
      if (this eq obj) true
      else {
        (obj.## == this.##) && (obj canEqual this) && (repr == obj.repr)
      }
    }
    case _ => false
  }

  override def hashCode(): Int = repr.##
}

trait D extends C {
  val name: String

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[D]

  override def equals(obj: Any): Boolean = obj match {
    case obj: D => {
      if (this eq obj) true
      else {
        (repr == obj.repr) && (obj canEqual this) && (name == obj.name)
      }
    }
    case _ => false
  }
}

object test2 extends App {
  val x = new C {
    override val repr: Int = 2
  }
  val y = new D {
    override val repr: Int = 2
    override val name: String = "kkk"
  }
  println(x.getClass)
  println(y.getClass)
  println(x == y)
  println(y == x)
}
