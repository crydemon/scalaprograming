import java.util.BitSet;

public class BitSetTest {

  public static void main(String[] args) {
    int [] array = new int [] {1,2,3,22,0,3,63, 100000000};
    BitSet bitSet = new BitSet();

    //将数组内容组bitmap
    for(int i=0;i<array.length;i++)
    {
      bitSet.set(array[i], true);
    }

    System.out.println("下面开始遍历BitSet：");
    for ( int i = 0; i < bitSet.size(); i++ ){
      System.out.println(bitSet.get(i) + "," + i);
    }
  }

}
