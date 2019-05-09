/**
 * @author wqkant
 */
public interface Property {
    //public：接口可以被其他接口继承，也可以被类实现，类与接口、接口与接口可能会形成多层级关系，采用public可以满足变量的访问范围；
    //static：如果变量不是static的，那么接口必须实例化才可以访问自己的变量，接口不能被实例化，故非static的变量是无效的；
    //final：如果变量不是final的，而方法是abstract的，因此接口中的方法又不可以修改变量值，虽然可以直接修改静态成员变量，但所有实现类对应的值都被修改了，此做法等同于抽象类，故需要final修饰成员变量；
    String name = "";

    /**
     * 不能使用toString This is done to avoid problems with multiple inheritance.
     *
     * @return
     */
    default String tostring() {
        return "Property (" + name + ")";
    }
}
