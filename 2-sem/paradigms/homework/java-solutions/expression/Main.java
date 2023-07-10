package expression;

import expression.exceptions.ExpressionParser;
import expression.exceptions.TripleParser;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        TripleParser parser = new ExpressionParser();
        Scanner in = new Scanner(System.in);
        AllExpressions a;
        try {
             a = (AllExpressions) parser.parse(in.nextLine());
        } catch (Exception e) {
            System.out.println(e.getMessage());
            return;
        }
        System.out.println("Expression: " + a.toString());

        System.out.println("x\tf");
        for (int i = 0; i <= 10; i++) {
            System.out.printf("%d\t", i);
            try {
                System.out.printf("%d", a.evaluate(i));
            } catch (Exception e) {
                System.out.printf("%s", e.getMessage());
            }
            System.out.println();
        }
    }
}
