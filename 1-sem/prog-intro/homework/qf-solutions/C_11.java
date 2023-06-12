import java.io.*;
import java.util.*;

public class CTask {
    private static final int[][] hidedNeigbourEdges = new int[][] {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
    private static final int[][] visibleNeigbourEdges = new int[][] {{1, 1}, {-1, 1}, {1, -1}, {-1, -1}};

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int w, h;
        w = in.nextInt();
        h = in.nextInt();
        String[] pattern = new String[h];

        Set<Edge> notPassedVisibleEdges = new HashSet<>(), notPassedHideEdges = new HashSet<>();
        Stack<IntPair> nodes = new Stack<>();

        for (int i = 0; i < h; i++) {
            pattern[i] = in.next();
            for (int j = 0; j < w; j++) {
                if (pattern[i].charAt(j) == 'X') {

                    notPassedVisibleEdges.add(new Edge(new IntPair(j, i), new IntPair(j + 1, i + 1)));
                    notPassedVisibleEdges.add(new Edge(new IntPair(j + 1, i), new IntPair(j, i + 1)));

                    if ((i + j) % 2 == 0) {
                        notPassedHideEdges.add(new Edge(new IntPair(j, i), new IntPair(j, i + 1)));
                        notPassedHideEdges.add(new Edge(new IntPair(j + 1, i), new IntPair(j + 1, i + 1)));
                    } else {
                        notPassedHideEdges.add(new Edge(new IntPair(j, i), new IntPair(j + 1, i)));
                        notPassedHideEdges.add(new Edge(new IntPair(j, i + 1), new IntPair(j + 1, i + 1)));
                    }

                    if (nodes.size() == 0) {
                        nodes.push(new IntPair(j, i));
                    }
                }
            }
        }

        if (nodes.size() == 0) {
            System.out.println(0);
            return;
        }

        boolean hide = false;

        System.out.println(notPassedVisibleEdges.size() + notPassedHideEdges.size() - 1);

        List<IntPair> graphPath = new ArrayList<>();

        while (notPassedVisibleEdges.size() > 0 || notPassedHideEdges.size() > 0) {
            IntPair now = nodes.peek();
            boolean foundEdge = false;
            if (hide) {
                for (int[] vector : hidedNeigbourEdges) {
                    IntPair to = new IntPair(now.x + vector[0], now.y + vector[1]);
                    Edge nowEdge = new Edge(now, to);
                    if (notPassedHideEdges.contains(nowEdge)) {

                        notPassedHideEdges.remove(nowEdge);
                        nodes.push(to);

                        foundEdge = true;
                        break;
                    }
                }
            } else {
                for (int[] vector : visibleNeigbourEdges) {
                    IntPair to = new IntPair(now.x + vector[0], now.y + vector[1]);
                    Edge nowEdge = new Edge(now, to);
                    if (notPassedVisibleEdges.contains(nowEdge)) {

                        notPassedVisibleEdges.remove(nowEdge);
                        nodes.push(to);

                        foundEdge = true;
                        break;
                    }
                }
            }

            if (!foundEdge) {
                graphPath.add(nodes.pop());
            }
            hide = !hide;
        }

        while (nodes.size() > 0) {
            graphPath.add(nodes.pop());
        }

        for (int i = 1; i < graphPath.size(); i++) {
            System.out.printf("%d %d", graphPath.get(i).x, graphPath.get(i).y);
            System.out.println();
        }
    }
}

class IntPair {
    public int x;
    public int y;

    public IntPair(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object that) {
        return this.x == ((IntPair)that).x && this.y == ((IntPair)that).y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}

class Edge {
    public IntPair from;
    public IntPair to;

    public Edge(IntPair from, IntPair to) {
        if ( from.x + from.y < to.x + to.y ) {
            this.from = from;
            this.to = to;
        } else if ( from.x + from.y > to.x + to.y) {
            this.from = to;
            this.to = from;
        } else {
            if (from.x < to.x) {
                this.from = from;
                this.to = to;
            } else {
                this.from = to;
                this.to = from;
            }
        }
    }

    @Override
    public boolean equals(Object that) {
        return this.from.equals(((Edge)that).from) && this.to.equals(((Edge)that).to);
    }

    @Override
    public int hashCode() {
        return  Objects.hash(this.from, this.to);
    }
}