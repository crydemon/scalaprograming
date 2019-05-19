import org.scalatest.Fact;

import java.util.ArrayList;
import java.util.List;


public class Leetcode {

    public Node connect(Node root) {
        if (root == null) return null;
        List<Node> list = new ArrayList<>();
        list.add(root);
        while (!list.isEmpty()) {
            int size = list.size();
            Node pre = null;
            for (int i = 0; i < size; i++) {
                Node curr = list.remove(0);
                if (pre != null) pre.next = curr;
                pre = curr;
                if (curr.left != null) list.add(curr.left);
                if (curr.right != null) list.add(curr.right);
            }
        }
        return root;
    }

    private int dfs(TreeNode node, int result) {
        if (node == null) return result;
        else if (node.left != null && node.right != null)
            return dfs(node.left, result * 10 + node.val) + dfs(node.right, result * 10 + node.val);
        else if (node.left != null) return dfs(node.left, result * 10 + node.val);
        else if (node.right != null) return dfs(node.right, result * 10 + node.val);
        else return result * 10 + node.val;
    }

    public int sumNumbers(TreeNode root) {
        return dfs(root, 0);
    }

    public static void main(String[] args) {

    }
}

class Node {
    public int val;
    public Node left;
    public Node right;
    public Node next;

    public Node() {
    }

    public Node(int _val, Node _left, Node _right, Node _next) {
        val = _val;
        left = _left;
        right = _right;
        next = _next;
    }
}

class TreeNode {
    int val;
    TreeNode left;
    TreeNode right;

    TreeNode(int x) {
        val = x;
    }
}
