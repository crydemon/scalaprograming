public class Test {
    public static void main(String[] args){
        Property p =  new A() ;
        System.out.println(p.getClass());
        System.out.println(p.tostring());

    }
}

class A implements Property {
    String name = "fksdjf";

    @Override
    public String tostring() {
        return "Property (" + name + ")";
    }
}
