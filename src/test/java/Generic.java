import com.oracle.jrockit.jfr.Producer;

import java.util.ArrayList;
import java.util.List;

public class Generic {

    //如果你想从一个数据类型里获取数据
    public void upperBound(List<? extends Number> list) {
        Object obj = list.get(
                0); //List<? extends Number>eList存放Number及其子类的对象，语句1取出Number（或者Number子类）对象直接赋值给Number类型的变量是符合java规范的。
        Number num = list.get(0);
        //list = new ArrayList<Integer>();
        //list.add(new Integer(1))//List<? extends Number>eList不能够确定实例化对象的具体类型，因此无法add具体对象至列表中，可能的实例化对象如下
//    Integet i = list.get(0); // compile error
//    list.add(new Integer(1)); // compile error
    }



    //如果你想把对象写入一个数据结构里
    public static void lowerBound(List<? super Number> l) {
        l.add(new Integer(1));
        l.add(new Float(2));//子类对象的引用可以赋值给父类对象的引用
        Object obj = l.get(0);
        //Integer x = l.get(0);无法确定sList中存放的对象的具体类型，因此sList.get获取的值存在不确定性，子类对象的引用无法赋值给兄弟类的引用，父类对象的引用无法赋值给子类的引用，因此语句错误
//    Number num = l.get(0); // compile error
        System.out.println(obj);
    }

    //  上界类型通配符：add方法受限
//  下界类型通配符：get方法受限
//  在Java中，泛型是作为语法糖出现的。在虚拟机层面，并不存在泛型这种类型，也不会对泛型进行膨胀，生成出类似于List<String>、List<Entry>之类的类型。在虚拟机看来，List<E>这个泛型类型只是普通的类型List而已，这种行为叫泛型擦除(Type Erasure)。
//
//  那么在Java中泛型是如何如何实现其目的的呢？Java的泛型充分利用了多态性。将无界(unbounded)的通配符(wildcard)理解为Object类型，因为Object类型是所有除标量(Scalar)以外，包括普通的数组和标量数组的类型的父类。将所有有上界(upper bound)的通配符理解为其上界类型例如<T extends CharSequence>将被理解为CharSequence类型。并在相应的地方自动生成checkcast字节码进行类型检查和转换，这样就既可以实现泛型，又不需要在字节码层面的进行改动来支持泛型。这样的泛型叫做伪泛型。
    public static void main(String[] args) {
        List<? extends BB> a = new ArrayList<>();
        List<? super BB> b = new ArrayList<>();
        //a.add(new BB());
        b.add(new BB());
        b.add(new CC());
        b.add(new DD());
        lowerBound(new ArrayList<>());
    }

//  最后看一下什么是PECS（
//  Producer Extends Consumer Super）原则，已经很好理解了： 频繁往外读取内容的，适合用上界Extends。 经常往里插入的，适合用下界Super。
//  List ：”存的时候只能选一个类型。“
//  List <? extends Fruit> 意思： List中所有元素都是Fruit的子类(包含本身)，


}
class AA {

}

class BB extends AA {

}

class CC extends BB {

}
class DD extends CC {

}