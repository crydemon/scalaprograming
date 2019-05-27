import org.scalatest.Fact;
import scala.Int;

import java.util.ArrayList;
import java.util.HashMap;
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
        if (node == null) {
            return result;
        } else if (node.left != null && node.right != null) {
            return dfs(node.left, result * 10 + node.val) + dfs(node.right, result * 10 + node.val);
        } else if (node.left != null) {
            return dfs(node.left, result * 10 + node.val);
        } else if (node.right != null) {
            return dfs(node.right, result * 10 + node.val);
        } else {
            return result * 10 + node.val;
        }
    }

    public int sumNumbers(TreeNode root) {
        return dfs(root, 0);
    }

    public int singleNumber(int[] nums) {
        HashMap<Integer, Integer> map = new HashMap<>();
        int result = 0;
        for (int num : nums) {
            int x = map.getOrDefault(num, 0);
            if (x == 1) {
                result -= num;
            } else if (x == 0) {
                result += num;
            }
            map.put(num, x + 1);
        }
        return result;
    }

    public void reorderList(ListNode head) {
        // Find the middle node
        ListNode slow = head, fast = head.next;
        while (fast != null && fast.next != null) {
            slow = slow.next;
            fast = fast.next.next;
        }
        // Reverse the second half
        ListNode head2 = reverse(slow);

        while (head != null && head2 != null) {
            ListNode tmp1 = head.next;
            ListNode tmp2 = head2.next;
            head2.next = head.next;
            head.next = head2;
            head = tmp1;
            head2 = tmp2;
        }
    }

    private ListNode reverse(ListNode n) {
        ListNode prev = null;
        ListNode cur = n;
        while (cur != null) {
            ListNode tmp = cur.next;
            cur.next = prev;
            prev = cur;
            cur = tmp;
        }
        return prev;
    }

    public List<Integer> preorderTraversal(TreeNode root) {
        return preorder(root, new ArrayList<>());
    }

    public List<Integer> preorder(TreeNode node, List<Integer> result) {
        if (node == null) {
            return result;
        }
        result.add(node.val);
        if (node.left != null) {
            preorder(node.left, result);
        }
        if (node.right != null) {
            preorder(node.right, result);
        }
        return result;
    }

    public List<Integer> postorderTraversal(TreeNode root) {
        return postorder(root, new ArrayList<>());
    }

    public List<Integer> postorder(TreeNode node, List<Integer> result) {
        if (node == null) {
            return result;
        }
        if (node.left != null) {
            postorder(node.left, result);
        }
        if (node.right != null) {
            postorder(node.right, result);
        }
        result.add(node.val);
        return result;
    }

    public static void main(String[] args) {
        int ans = new Leetcode().singleNumber(new int[]{0, 1, 0, 1, 0, 1, 99});
        System.out.println(ans);
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

class ListNode {
    int val;
    ListNode next;

    ListNode(int x) {
        val = x;
    }
}
