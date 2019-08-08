import org.junit.Test;
import org.scalatest.Fact;
import scala.Int;

import java.util.*;


public class Leetcode {

    public int lengthOfLongestSubstring(String s) {
        int[] hash = new int[256];
        Arrays.fill(hash, -1);
        int result = 0;
        int curMax = 0;
        int start = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (hash[ch] == -1) {
                curMax++;
            } else {
                curMax = i - hash[ch];
                for (int j = start; j < hash[ch]; j++) {
                    hash[s.charAt(j)] = -1;
                }
                start = hash[ch] + 1;
                hash[ch] = -1;
            }
            hash[ch] = i;
            result = Math.max(curMax, result);
        }
        return result;
    }

    public String longestCommonPrefix(String[] strs) {
        if (strs == null || strs.length == 0) return "";
        String result = strs[0];
        for (String str : strs) {
            while (!str.startsWith(result)) {
                result = result.substring(0, result.length() - 1);
                if (result == null) {
                    return "";
                }
            }
        }
        return result;
    }

    public boolean checkInclusion(String s1, String s2) {
        if (s1.length() > s2.length()) return false;
        //滑动窗口
        int[] win1 = new int[26];
        int[] win2 = new int[26];
        for (int i = 0; i < s1.length(); i++) {
            win1[s1.charAt(i) - 'a']++;
            win2[s2.charAt(i) - 'a']++;
        }
        if (Arrays.equals(win1, win2)) {
            return true;
        }
        for (int i = 0; i < s2.length() - s1.length(); i++) {
            win2[s2.charAt(i) - 'a']--;
            win2[s2.charAt(s1.length() + i) - 'a']++;
            if (Arrays.equals(win1, win2)) {
                return true;
            }
        }
        return false;
    }

    public String multiply(String num1, String num2) {
        //存储当前累乘的值
        int[] acc = new int[num1.length() + num2.length()];
        for (int i = num1.length() - 1; i >= 0; i--) {
            int x = num1.charAt(i) - '0';
            for (int j = num2.length() - 1; j >= 0; j--) {
                int y = num2.charAt(j) - '0';
                int sum = acc[i + j + 1] + x * y;
                acc[i + j] += sum / 10;//进位
                acc[i + j + 1] = sum % 10;//更新当前位
            }
        }
        System.out.println(Arrays.toString(acc));
        String result = "";
        for (int i : acc) {
            if (i == 0 && result == "") continue;
            result += i;
        }
        return result;
    }

    public String reverseWords(String s) {
        String result = "";
        for (int i = 0; i < s.length(); ) {
            while (i < s.length() && s.charAt(i) == ' ') {
                i++;
            }
            String word = "";
            while (i < s.length() && s.charAt(i) != ' ') {
                word += s.charAt(i++);
            }
            if (word == "") break;
            if (result.isEmpty()) {
                result = word;
            } else {
                result = word + " " + result;
            }
        }
        return result;
    }

    public String simplifyPath(String path) {
        String[] strs = path.split("/");
        System.out.println(Arrays.toString(strs));
        Stack<String> stack = new Stack<>();
        for (String str : strs) {
            if (str.isEmpty()) continue;
            if (str.equals(".")) {
                continue;
            } else if (str.equals("..")) {
                if (stack.isEmpty()) return "/";
                stack.pop();
            } else {
                stack.push(str);
            }
        }
        String result = "";
        while (!stack.isEmpty()) {
            String s = stack.pop();
            if (result.isEmpty()) {
                result = s;
            } else {
                result = s + "/" + result;
            }
        }
        return "/" + result;
    }

    public List<String> restoreIpAddresses(String s) {
        List<String> result = new ArrayList<>();
        helper1(s, 0, 0, result, "");
        return result;
    }

    public void helper1(String s, int pos, int num, List<String> result, String cur) {
        if (num > 4 || pos > s.length()) {
            return;
        } else if (pos == s.length() && num == 4) {
            result.add(cur);
        } else {
            for (int i = pos + 1; i < pos + 4 && i <= s.length(); i++) {
                String x = s.substring(pos, i);
                Integer xInt = Integer.valueOf(x);
                if (xInt <= 255 && x.equals(String.valueOf(xInt))) {
                    if (cur == "") {
                        helper1(s, i, num + 1, result, x);
                    } else {
                        helper1(s, i, num + 1, result, cur + "." + x);
                    }
                }
            }
        }
    }

    public List<List<Integer>> threeSum(int[] nums) {
        List<List<Integer>> result = new ArrayList<>();
        Arrays.sort(nums);
        for (int i = 0; i + 2 < nums.length; i++) {
            int target = -nums[i];
            if (i > 0 && nums[i] == nums[i - 1]) continue;
            int j = i + 1;
            int k = nums.length - 1;
            while (j < k) {
                int sum = nums[j] + nums[k];
                if (sum == target) {
                    result.add(Arrays.asList(nums[i], nums[j], nums[k]));
                    while (j < k && nums[++j] == nums[j + 1]) ;
                    while (j < k && nums[--k] == nums[k - 1]) ;
                } else if (sum < target) {
                    j++;
                } else {
                    k--;
                }
            }
        }
        return result;
    }

    public int maxAreaOfIsland(int[][] grid) {
        int maxArea = 0;
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[0].length; j++) {
                if (grid[i][j] == 1) {
                    maxArea = Math.max(maxArea, helper3(i, j, grid));
                }
            }
        }
        return maxArea;
    }

    public ListNode addTwoNumbers(ListNode l1, ListNode l2) {
        ListNode dummy = new ListNode(-1);
        ListNode result = dummy;
        int carry = 0;
        while (l1 != null || l2 != null || carry != 0) {
            int cur = 0;
            if (l2 != null) {
                cur += l2.val;
                l2 = l2.next;
            }
            if (l1 != null) {
                cur += l1.val;
                l1 = l1.next;
            }
            cur += carry;
            dummy.next = new ListNode(cur % 10);
            carry = (cur) / 10;
            dummy = dummy.next;
        }
        return result.next;
    }

    public int helper3(int i, int j, int[][] grid) {
        if (i < 0 || i > grid.length || j < 0 || j > grid[0].length || grid[i][j] == 0) return 0;
        grid[i][j] = 0;
        return 1 + helper3(i - 1, j, grid) + helper3(i + 1, j, grid) + helper3(i, j - 1, grid) + helper3(i, j + 1, grid);
    }

    public int search(int[] nums, int target) {
        int left = 0;
        int right = nums.length - 1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            System.out.println(mid);
            if (nums[left] <= nums[mid]) {
                int index = Arrays.binarySearch(nums, left, mid + 1, target);
                if (index >= 0) return index;
                else {
                    left = mid + 1;
                }
            } else {
                System.out.println(right + 1);
                int index = Arrays.binarySearch(nums, mid, right + 1, target);
                if (index >= 0) return index;
                else {
                    right = mid - 1;
                }
            }
        }
        return -1;
    }

//    public ListNode sortList(ListNode head) {
//        if(head == null || head.next == null) return head;
//        ListNode pre = null, slow = head, fast = head;
//        while (fast != null && fast.next != null) {
//            pre = slow;
//            slow = slow.next;
//            fast = fast.next.next;
//        }
//        pre.next = null;
//        ListNode l1 = sortList(head);
//        ListNode l2 = sortList(slow);
//        return merge(l1, l2);
//    }
//    private ListNode merge(ListNode l1, ListNode l2){
//        ListNode l = new ListNode(0), p = l;
//        while (l1 != null & l2 != null) {
//            if(l1.val < l2.val) {
//                p.next = l1;
//                l1 = l1.next;
//            } else {
//                p.next = l2;
//                l2 = l2.next;
//            }
//            p = p.next;
//        }
//        if(l1 != null) p.next = l1;
//        if(l2 != null) p.next = l2;
//        return l.next;
//    }

    public ListNode sortList(ListNode head) {
        quickSort(head, null);
        return head;
    }

    public void quickSort(ListNode head, ListNode end) {
        if (head == end) return;
        ListNode partition = helper4(head);
        quickSort(head, partition);
        quickSort(partition.next, end);
    }

    public ListNode helper4(ListNode head) {
        ListNode slow = head;
        ListNode fast = head.next;
        while (fast != null) {
            if (fast.val < head.val) {
                slow = slow.next;
                swapListVal(slow, fast);
            }
            fast = fast.next;
        }
        swapListVal(slow, head);
        return slow;
    }

    public void swapListVal(ListNode x1, ListNode x2) {
        int tmp = x1.val;
        x1.val = x2.val;
        x2.val = tmp;
    }

    public ListNode detectCycle(ListNode head) {
        if (head == null) return null;
        ListNode slow = head;
        ListNode fast = head;
        while (slow != null && fast.next != null) {
            slow = slow.next;
            fast = fast.next.next;
            if (slow == fast) { //s = x + y // 2s = x + 2 * y + z
                slow = head;
                while (slow != fast) {
                    slow = slow.next;
                    fast = fast.next;
                }
                return slow;
            }
        }
        return null;
    }

    public ListNode getIntersectionNode(ListNode headA, ListNode headB) {
        ListNode p1 = headA;
        ListNode p2 = headB;
        while (p1 != null && p2 != null) {
            p1 = p1.next;
            p2 = p2.next;
        }
        if (p1 == null) {
            while (p2 != null) {
                headB = headB.next;
                p2 = p2.next;
            }
        }
        if (p2 == null) {
            while (p1 != null) {
                headA = headA.next;
                p1 = p1.next;
            }
        }
        while (headA != headB) {
            headA = headA.next;
            headB = headB.next;
        }
        return headA;
    }

    public ListNode mergeKLists(ListNode[] lists) {
        return helper6(lists, 0, lists.length - 1);
    }

    public ListNode helper6(ListNode[] lists, int left, int right) {
        if (left > right) return null;
        if (left == right) return lists[left];
        int mid = left + (right - left) / 2;
        ListNode l1 = helper6(lists, left, mid);
        ListNode l2 = helper6(lists, mid + 1, right);
        return mergeTwoList(l1, l2);
    }

    public ListNode mergeTwoList(ListNode l1, ListNode l2) {
        if (l1 == null) return l2;
        if (l2 == null) return l1;
        if (l1.val < l2.val) {
            l1.next = mergeTwoList(l1.next, l2);
            return l1;
        } else {
            l2.next = mergeTwoList(l1, l2.next);
            return l2;
        }
    }

    public int findLengthOfLCIS(int[] nums) {
        if (nums.length == 0) return 0;
        int count = 1;
        int max = 0;
        for (int i = 1; i < nums.length; i++) {
            if (nums[i - 1] < nums[i]) {
                count++;
            } else {
                max = Math.max(max, count);
                count = 1;
            }
        }
        return Math.max(max, count);
    }

    public int findKthLargest(int[] nums, int k) {
        return search(k, 0, nums.length - 1, nums);
    }


    public int search(int k, int left, int right, int[] nums) {
        int x = partition(left, right, nums);
        if (x == -1) return -1;
        if (x == k - 1) return nums[x];
        else if (x > k - 1) return search(k, 0, x - 1, nums);
        else return search(k, x + 1, right, nums);
    }

    public int partition(int left, int right, int[] nums) {
        if (left > right) return -1;
        int x = left;
        while (left <= right) {//因为right表示的是左边第right大，所以必须要加上等号
            System.out.println(Arrays.toString(nums));
            System.out.println(left + "," + right);
            if (nums[left] >= nums[x]) left++;
            if (nums[right] < nums[x]) right--;
            if (left < right && nums[left] < nums[right]) {
                swap(left, right, nums);
            }
        }
        swap(x, right, nums);
        System.out.println(right);
        System.out.println(Arrays.toString(nums));
        return right;
    }

    public void swap(int i, int j, int[] nums) {
        int tmp = nums[i];
        nums[i] = nums[j];
        nums[j] = tmp;
    }

    public int longestConsecutive(int[] nums) {
        if (nums.length == 0) return 0;
        Arrays.sort(nums);
        int maxLen = 1;
        int curLen = 1;
        for (int i = 1; i < nums.length; i++) {
            if (nums[i - 1] == nums[i]) {
                curLen++;
            } else {
                maxLen = Math.max(maxLen, curLen);
                curLen = 1;
            }
        }
        maxLen = Math.max(maxLen, curLen);
        return maxLen;
    }

    public String getPermutation(int n, int k) {
        int[] fac = new int[n + 1];
        Arrays.fill(fac, 1);
        ArrayList<Integer> list = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            list.add(i);
            fac[i] = i * fac[i - 1];
        }
        System.out.println(Arrays.toString(fac));
        StringBuffer sb = new StringBuffer();
        --k;
        while (--n >= 0) {
            if (list.isEmpty()) return sb.toString();
            int mod = k / fac[n];
            sb.append(list.get(mod));
            list.remove(mod);
            k = k % fac[n];

        }
        return sb.toString();
    }

    public int findCircleNum(int[][] M) {
        int[] root = new int[M.length];
        for (int i = 0; i < M.length; i++) {
            root[i] = i;
        }
        int result = M.length;
        for (int i = 0; i < M.length; i++) {
            for (int j = i + 1; j < M.length; j++) {
                if (M[i][j] == 1) {
                    int p1 = getRoot(root, i);
                    int p2 = getRoot(root, j);
                    if (p1 != p2) {
                        result--;
                        root[p2] = p1;
                    }
                }
            }
        }
        return result;
    }

    public int getRoot(int[] root, int child) {
        while (root[child] != child) {
            root[child] = root[root[child]];
            child = root[child];
        }
        return child;
    }

    class Solution {
        public int findCircleNum(int[][] M) {
            int len = M.length;
            boolean[] flag = new boolean[len]; //默认为false
            int count = 0;
            for (int i = 0; i < len; i++) {
                if (!flag[i]) {
                    dfs(i, M, flag);
                    count++;
                }
            }
            return count;
        }

        void dfs(int i, int[][] M, boolean[] flag) {
            flag[i] = true;
            for (int j = 0; j < M.length; j++) {
                if (!flag[j] && M[i][j] == 1) {
                    dfs(j, M, flag);
                }
            }
        }
    }

    public int[][] merge(int[][] intervals) {
        if (intervals.length <= 1) return intervals;
        Arrays.sort(intervals, Comparator.comparingInt(interval -> interval[0]));
        List<int[]> result = new ArrayList<>();
        int[] newInterval = intervals[0];

        result.add(newInterval);
        for (int[] interval : intervals) {
            if (interval[0] <= newInterval[1]) {
                newInterval[1] = Math.max(newInterval[1], interval[1]);
            } else {
                newInterval = interval;
                result.add(newInterval);
            }
        }
        result.forEach(x -> {
            System.out.println(x[0] + "," + x[1]);
        });
        return result.toArray(new int[result.size()][]);
    }

    public int trap(int[] height) {
        int result = 0;
        int left = 0;
        int right = height.length - 1;
        int level = 0;
        int lower = 0;
        while (left < right) {
            //维护长的那一端， 并选出短的那一端
            lower = height[height[left] < height[right] ? left++ : right--];
            //维护次高的那一端
            level = Math.max(level, lower);
            //次长减去当前高度就是这个格子能装的水了
            result += level - lower;
        }
        return result;
    }

    public ListNode mergeTwoLists(ListNode l1, ListNode l2) {
        ListNode dummy = new ListNode(-1);
        ListNode cur = dummy;
        while (l1 != null || l2 != null) {
            if (l1 == null) {
                cur.next = l2;
                break;
            } else if (l2 == null) {
                cur.next = l1;
                break;
            } else if (l1.val < l2.val) {
                cur.next = l1;
                l1 = l1.next;
            } else {
                cur.next = l2;
                l2 = l2.next;
            }
            cur = cur.next;
        }
        return dummy.next;
    }

    public ListNode reverseList(ListNode head) {
//        ListNode p1 = null;
//        ListNode p2 = head;
//        while ( p2 != null) {
//            p2 = head.next;
//            head.next = p1;
//            p1 = head;
//            head = p2;
//        }
//        return p1;
        if (head == null || head.next == null) {
            return head;
        }
        ListNode result = reverseList(head.next);
        ListNode last = head.next;
        last.next = head;
        head.next = null;
        return result;

    }

    public TreeNode lowestCommonAncestor(TreeNode root, TreeNode p, TreeNode q) {
        if (root == null || root == p || root == q) {
//            if(root != null) System.out.println(root.val + "------");
//            else  System.out.println("null");
            return root;
        }
        System.out.println(root.val);
        TreeNode left = lowestCommonAncestor(root.left, p, q);
        TreeNode right = lowestCommonAncestor(root.right, p, q);
        if (left != null && right != null) return root;
        return left == null ? right : left;
    }

    public List<List<Integer>> zigzagLevelOrder(TreeNode root) {
        if (root == null) return null;
        List<List<Integer>> result = new ArrayList<>();
        Queue<TreeNode> queue = new LinkedList<>();
        queue.add(root);
        boolean flag = false;
        while (!queue.isEmpty()) {
            ArrayList<Integer> curLayer = new ArrayList<>();
            int size = queue.size();
            for (int i = 0; i < size; i++) {
                TreeNode node = queue.poll();
                curLayer.add(node.val);
                if (node.left != null) {
                    queue.add(node.left);
                }
                if (node.right != null) {
                    queue.add(node.right);
                }
            }
            if (flag) Collections.reverse(curLayer);
            result.add(curLayer);
            flag = !flag;

        }
        return result;
    }

    public int maxProfit(int[] prices) {
        int max = 0;
        int min = prices[0];
        for (int i = 0; i < prices.length; i++) {
            min = Math.min(prices[i], min);
            max = Math.max(max, prices[i] - min);
        }
        return max;
    }

    public int maxProfitII(int[] prices) {
        int max = 0;
        for (int i = 1; i < prices.length; i++) {
            int tmp = prices[i] - prices[i - 1];
            if (tmp > 0) {
                max += tmp;
            }
        }
        return max;
    }

    public int maximalSquare(char[][] matrix) {
        if (matrix == null || matrix.length == 0) return 0;
        int max = 0;
        int[][] dp = new int[matrix.length][matrix[0].length];
        for (int i = 0; i < matrix.length; i++) {
            dp[i][0] = matrix[i][0] - '0';
            max = Math.max(max, dp[i][0]);
        }
        for (int i = 0; i < matrix[0].length; i++) {
            dp[0][i] = matrix[0][i] - '0';
            max = Math.max(max, dp[0][i]);
        }
        for (int i = 1; i < matrix.length; i++) {
            for (int j = 1; j < matrix[0].length; j++) {
                if (matrix[i][j] == '1') {
                    dp[i][j] = Math.min(dp[i - 1][j - 1], Math.min(dp[i - 1][j], dp[i][j - 1])) + 1;
                    max = Math.max(dp[i][j], max);
                }
            }
        }
        return max * max;
    }

    public int maxSubArray(int[] nums) {
        int max = nums[0];
        int pre = nums[0];
        for (int i = 1; i < nums.length; i++) {
            if (pre < 0) pre = 0;
            pre = pre + nums[i];
            max = Math.max(pre, max);
        }
        return max;
    }

    @Test
    public void test1() {
        String e = "1";
        String x = new String("1");

        System.out.println(("1" == x.intern()) ? "true" : "false");
    }

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

    public ListNode insertionSortList(ListNode head) {
        if (head == null) return null;
        ListNode unsorted = head;
        ListNode result = new ListNode(Integer.MIN_VALUE);
        ListNode pre = result;
        ListNode next = null;
        while (unsorted != null) {
            next = unsorted.next;
            while (pre.next != null && pre.next.val < unsorted.val) {
                pre = pre.next;
            }
            unsorted.next = pre.next;
            pre.next = unsorted;
            pre = result;
            unsorted = next;
        }
        return result.next;
    }

    public int evalRPN(String[] tokens) {
        if (tokens == null) return 0;
        Stack<Integer> stack = new Stack<>();
        for (String s : tokens) {
            if (s.equals("+")) {
                int x1 = stack.pop();
                int x2 = stack.pop();
                stack.push(x1 + x2);
            } else if (s.equals("-")) {
                int x1 = stack.pop();
                int x2 = stack.pop();
                stack.push(x1 - x2);
            } else if (s.equals("*")) {
                int x1 = stack.pop();
                int x2 = stack.pop();
                stack.push(x1 * x2);
            } else if (s.equals("/")) {
                int x1 = stack.pop();
                int x2 = stack.pop();
                stack.push(x1 / x2);
            } else {
                stack.push(Integer.valueOf(s));
            }
        }
        return stack.pop();
    }

    public String convertToTitle(int n) {
        String result = "";
        while (n-- > 0) {
            result = (char) ('A' + (n % 26)) + result;
            n = n / 26;
        }
        return result;
    }

    public static void main(String[] args) {
        System.out.println(new Leetcode().convertToTitle(701));
        //System.out.println(new Leetcode().evalRPN(new String[]{"2", "1", "+", "3", "*"}));
//        ListNode head = new ListNode(4);
//        head.next = new ListNode(2);
//        head.next.next = new ListNode(1);
//        head.next.next.next = new ListNode(3);
//        ListNode result = new Leetcode().insertionSortList(head);
//        while (result != null) {
//            System.out.println(result.val);
//            result = result.next;
//        }
//        int ans = new Leetcode().singleNumber(new int[]{0, 1, 0, 1, 0, 1, 99});
//        System.out.println(ans);
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

class BSTIterator {

    private Stack<TreeNode> stack = new Stack<TreeNode>();

    public BSTIterator(TreeNode root) {
        pushAll(root);
    }

    /**
     * @return whether we have a next smallest number
     */
    public boolean hasNext() {
        return !stack.isEmpty();
    }

    /**
     * @return the next smallest number
     */
    public int next() {
        TreeNode tmpNode = stack.pop();
        pushAll(tmpNode.right);
        return tmpNode.val;
    }

    private void pushAll(TreeNode node) {
        while (node != null) {
            stack.push(node);
            node = node.left;
        }
    }
}
