package expression;

public class Add extends AbstractBinaryExpression {

    public Add(AllExpressions first, AllExpressions second) {
        super(first, second, "+",  10, true);
    }

    @Override
    public int evaluate(int variable) {
        return first.evaluate(variable) + second.evaluate(variable);
    }

    @Override
    public double evaluate(double variable) {
        return first.evaluate(variable) + second.evaluate(variable);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) + second.evaluate(x, y, z);
    }
}
/**
 * Exception in thread "main" java.lang.AssertionError: Invalid toMiniString
 *      expected: 10 * (3 / 2)
 *        actual: 10 * 3 / 2
 *         at base.Asserts.error(Asserts.java:75)
 *         at base.Asserts.assertTrue(Asserts.java:41)
 *         at expression.ExpressionTester.lambda$checkToString$4(ExpressionTester.java:93)
 *         at base.TestCounter.lambda$test$0(TestCounter.java:58)
 *         at base.TestCounter.lambda$testV$2(TestCounter.java:71)
 *         at base.Log.silentScope(Log.java:40)
 *         at base.TestCounter.testV(TestCounter.java:70)
 *         at base.TestCounter.test(TestCounter.java:57)
 *         at expression.ExpressionTester.checkToString(ExpressionTester.java:93)
 *         at expression.ExpressionTester.checkEqualsAndToString(ExpressionTester.java:69)
 *         at expression.ExpressionTester$Test.lambda$test$1(ExpressionTester.java:179)
 *         at base.TestCounter.lambda$test$0(TestCounter.java:58)
 *         at base.TestCounter.lambda$testV$2(TestCounter.java:71)
 *         at base.Log.silentScope(Log.java:40)
 *         at base.TestCounter.testV(TestCounter.java:70)
 *         at base.TestCounter.test(TestCounter.java:57)
 *         at expression.ExpressionTester$Test.test(ExpressionTester.java:177)
 *         at java.base/java.util.ArrayList.forEach(ArrayList.java:1511)
 *         at expression.ExpressionTester.lambda$test$0(ExpressionTester.java:61)
 *         at base.Log.lambda$action$0(Log.java:18)
 *         at base.Log.silentScope(Log.java:40)
 *         at base.Log.scope(Log.java:31)
 *         at base.Log.scope(Log.java:24)
 *         at expression.ExpressionTester.test(ExpressionTester.java:61)
 *         at expression.ExpressionTest.lambda$v$0(ExpressionTest.java:23)
 *         at base.Selector.lambda$test$2(Selector.java:79)
 *         at base.Log.lambda$action$0(Log.java:18)
 *         at base.Log.silentScope(Log.java:40)
 *         at base.Log.scope(Log.java:31)
 *         at base.Log.scope(Log.java:24)
 *         at base.Selector.lambda$test$3(Selector.java:79)
 *         at java.base/java.util.ArrayList.forEach(ArrayList.java:1511)
 *         at base.Selector.test(Selector.java:79)
 *         at base.Selector.main(Selector.java:51)
 *         at expression.ExpressionTest.main(ExpressionTest.java:27)
 */