package expression.generic.generators;

import expression.generic.*;

import java.math.BigInteger;

public class BigIntegerGenerator extends AbstractGenerator<BigInteger> {
    @Override
    protected BigInteger add(BigInteger x, BigInteger y) {
        return x.add(y);
    }

    @Override
    protected BigInteger divide(BigInteger x, BigInteger y) {
        return x.divide(y);
    }

    @Override
    protected BigInteger module(BigInteger x, BigInteger y) {
        return x.mod(y);
    }

    @Override
    protected BigInteger multiply(BigInteger x, BigInteger y) {
        return x.multiply(y);
    }

    @Override
    protected BigInteger subtract(BigInteger x, BigInteger y) {
        return x.subtract(y);
    }

    @Override
    protected BigInteger abs(BigInteger x) {
        return x.abs();
    }

    @Override
    protected BigInteger negate(BigInteger x) {
        return x.negate();
    }

    @Override
    protected BigInteger square(BigInteger x) {
        return x.multiply(x);
    }

    @Override
    public GenericExpression<BigInteger> getConst(String value) {
        return new GenericConst<>(new BigInteger(value));
    }

    @Override
    public GenericExpression<BigInteger> getVariable(String value) {
        return new GenericVariable<>(value, x -> x);
    }

    @Override
    public BigInteger valueOf(int value) {
        return new BigInteger(String.valueOf(value));
    }
}
