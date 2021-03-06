package users {
  package administrators {

    class NormalUser

  }

  package normalusers {

    class NormalUser

  }

}


//包对象中可以定义任何内容，而不仅仅是变量和方法。 例如，包对象经常用于保存包级作用域的类型别名和隐式转换。 包对象甚至可以继承 Scala 的类和特质。
//
//按照惯例，包对象的代码通常放在名为 package.scala 的源文件中。
//
//每个包都允许有一个包对象。 在包对象中的任何定义都被认为是包自身的成员。


//如果包对象的作用仅仅限于伴随对象那样，那scala 2.8完全没有必要引入这种特性。
// 实际上包对象最重要的用途是兼容旧的类库，或者为某些数据类型提供增强版本。
//
//比如在scala 2.7.7中，List是定义在scala包下的一个不可变集合类。
// 这样做的目的是每次使用List的时候不需要显式地导入包名，因为List是一个使用很频繁的类。



//注意 users 目录是包含在 scala 目录中的，该包中包含有多个 Scala 文件。
// 包中的每个 Scala 文件都可以具有相同的包声明。 声明包的另一种方式是使用大括号：
//如你所见，这允许包嵌套并提供了对范围和封装的更好控制。