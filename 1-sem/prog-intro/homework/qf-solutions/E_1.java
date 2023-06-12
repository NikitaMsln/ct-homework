import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;

public class ETask {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int n = in.nextInt(), m = in.nextInt();
        Tree[] nodes = new Tree[n+1];
        int[] teams = new int[m];

        for (int i = 0; i < n - 1; i++) {
            int node1 = in.nextInt(), node2 = in.nextInt();

            if (nodes[node1] == null) {
                nodes[node1] = new Tree(0, node1);
            }
            if (nodes[node2] == null) {
                nodes[node2] = new Tree(0, node2);
            }

            Tree.setEdge(nodes[node1], nodes[node2]);
        }

        if (n == 1) {
            nodes[1] = new Tree(0, 1);
        }

        for (int i = 0; i < m; i++) {
            teams[i] = in.nextInt();
            nodes[teams[i]].value = 1;
        }

        if (m == 1) {
            System.out.println("YES");
            System.out.println(teams[0]);
            return;
        }

        TreeList longestPaths = new TreeList();
        int deep = nodes[teams[0]].findDeepestChildByValue(null, 1, longestPaths);

        TreeList longestPath = new TreeList();
        longestPath.add(longestPaths.get(longestPaths.size() - 1));

        for (int i = longestPaths.size() - 2; i >= 0; i--) {
            if (longestPath.get(longestPath.size() - 1).hasChild(longestPaths.get(i))) {
                longestPath.add(longestPaths.get(i));
            }
        }

        if (longestPath.size() % 2 == 0) {
            System.out.println("NO");
            return;
        }

        Tree city = longestPath.get(longestPath.size() / 2);
        if (city.compareDeepAllChildByValue(null, 1, longestPath.size() / 2, 0)) {
            System.out.println("YES");
            System.out.println(city.number);
        } else {
            System.out.println("NO");
        }
    }
}

class Tree {
    private TreeList childs = new TreeList();
    public int value, number;

    public Tree(int value, int number) {
        this.value = value;
        this.number = number;
    }

    public static void setEdge(Tree node1, Tree node2) {
        node1.childs.add(node2);
        node2.childs.add(node1);
    }

    public boolean hasChild(Tree child) {
        for (int i = 0; i < childs.size(); i++) {
            if (child == childs.get(i)) {
                return true;
            }
        }
        return false;
    }

    public int findDeepestChildByValue(Tree parent, int value, TreeList resultPath) {
        int maxDeep = -1;
        resultPath.add(this);
        for (int i = 0; i < childs.size(); i++) {
            if (childs.get(i) != parent) {
                int deep = childs.get(i).findDeepestChildByValue(this, value, resultPath);
                if (deep > maxDeep) {
                    maxDeep = deep;
                } else if (deep > 0) {
                    resultPath.removeLast(resultPath.size() - deep);
                }
            }
        }

        if (maxDeep < 0) {
            if (this.value == value) {
                return 1;
            } else {
                resultPath.removeLast(resultPath.size() - 1);
                return -1;
            }
        }

        return maxDeep + 1;
    }

    public boolean compareDeepAllChildByValue(Tree parent, int value, int deep, int nowDeep) {
        for (int i = 0; i < childs.size(); i++) {
            if (childs.get(i) != parent && !childs.get(i).compareDeepAllChildByValue(this, value, deep, nowDeep + 1)) {
                return false;
            }
        }
        if (this.value == value && nowDeep != deep) {
            return false;
        }
        return true;
    }
}

class TreeList {
    private Tree[] array = new Tree[10];
    private int size = 0;

    public TreeList() {
    }

    public Tree get(int index) throws IndexOutOfBoundsException {
        if (index >= size) {
            throw new IndexOutOfBoundsException("Index out of bounds TreeList");
        }
        return array[index];
    }

    public void add(Tree elem) {
        if (size >= array.length) {
            array = Arrays.copyOf(array, array.length * 2);
        }
        array[size] = elem;
        size++;
    }

    public void removeLast(int index) {
        if (index > size || index < 0) {
            throw new IndexOutOfBoundsException("Index out of bounds TreeList");
        }
        size = index;
    }

    public int size() {
        return size;
    }
}