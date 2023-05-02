function Const(value) {
    this.value = value;
}

Const.prototype.toString = Const.prototype.prefix = Const.prototype.postfix = function () {
    return this.value.toString();
}
Const.prototype.evaluate = function (x, y, z) {
    return this.value;
}
Const.prototype.diff = function (name) {
    return new Const(0);
}

function Variable(name) {
    this.name = name;
}

Variable.prototype.toString = Variable.prototype.prefix = Variable.prototype.postfix = function () {
    return this.name;
}
const VARIABLES = {
    "x": 0,
    "y": 1,
    "z": 2
}
Variable.prototype.evaluate = function (...args) {
    return args[VARIABLES[this.name]];
}
Variable.prototype.diff = function (name) {
    if (name === this.name) {
        return new Const(1);
    }
    return new Const(0);
}

function Operator(name, operator, ...args) {
    this.name = name;
    this.operator = operator;
    this.values = args;
}

Operator.prototype.toString = function () {
    // [].join(' ')
    return this.values.reduce((result, item) => result + item + " ", "") + this.name;
}
Operator.prototype.prefix = function () {
    return "(" + this.name + this.values.reduce((result, item) => result + " " + item.prefix(), "") + ")";
}
Operator.prototype.postfix = function () {
    return "(" + this.values.reduce((result, item) => result + item.postfix() + " ", "") + this.name + ")";
}
Operator.prototype.evaluate = function (x, y, z) {
    let elements = [];
    this.values.forEach((item) => elements.push(item.evaluate(x, y, z)));
    return this.operator(...elements);
}

function Add(first, second) {
    Operator.call(this, "+", (x, y) => x + y, first, second);
}

Add.prototype = Object.create(Operator.prototype);
Add.prototype.diff = function (name) {
    return new Add(this.values[0].diff(name), this.values[1].diff(name));
}

function Subtract(first, second) {
    Operator.call(this, "-", (x, y) => x - y, first, second);
}

Subtract.prototype = Object.create(Operator.prototype);
Subtract.prototype.diff = function (name) {
    return new Subtract(this.values[0].diff(name), this.values[1].diff(name));
}

function Multiply(first, second) {
    Operator.call(this, "*", (x, y) => x * y, first, second);
}

Multiply.prototype = Object.create(Operator.prototype);
Multiply.prototype.diff = function (name) {
    return new Add(
        new Multiply(this.values[0].diff(name), this.values[1]),
        new Multiply(this.values[0], this.values[1].diff(name))
    );
}

function Divide(first, second) {
    Operator.call(this, "/", (x, y) => x / y, first, second);
}

Divide.prototype = Object.create(Operator.prototype);
Divide.prototype.diff = function (name) {
    return new Divide(
        new Subtract(
            new Multiply(this.values[0].diff(name), this.values[1]),
            new Multiply(this.values[0], this.values[1].diff(name))
        ),
        new Multiply(
            this.values[1],
            this.values[1]
        )
    );
}

function Negate(value) {
    Operator.call(this, "negate", x => -x, value);
}

Negate.prototype = Object.create(Operator.prototype);
Negate.prototype.diff = function (name) {
    return new Negate(this.values[0].diff(name));
}

function ReverseDiff(value, name) {
    return new Divide(
        new Negate(value.diff(name)),
        new Multiply(value, value)
    );
}

function SumrecN(...values) {
    Operator.call(
        this,
        "sumrec" + values.length,
        (...args) => args.reduce((result, item) => result + 1 / item, 0),
        ...values
    )
}

SumrecN.prototype = Object.create(Operator.prototype);
SumrecN.prototype.diff = function (name) {
    return this.values.reduce((result, item) => new Add(result, ReverseDiff(item, name)), new Const(0));
}

const Sumrec2 = SumrecN;
const Sumrec3 = SumrecN;
const Sumrec4 = SumrecN;
const Sumrec5 = SumrecN;

function HMeanN(...values) {
    Operator.call(
        this,
        "hmean" + values.length,
        (...args) => values.length / args.reduce((result, item) => result + 1 / item, 0),
        ...values
    )
}

HMeanN.prototype = Object.create(Operator.prototype);
HMeanN.prototype.diff = function (name) {
    return new Multiply(new Const(this.values.length), ReverseDiff(this.values.reduce((result, item) => new Add(result, new Divide(new Const(1), item)), new Const(0)), name));
}

const HMean2 = HMeanN;
const HMean3 = HMeanN;
const HMean4 = HMeanN;
const HMean5 = HMeanN;

function Meansq(...values) {
    Operator.call(
        this,
        "meansq",
        (...args) => args.reduce((result, item) => result + item * item, 0) / values.length,
        ...values
    );
}

Meansq.prototype = Object.create(Operator.prototype);
Meansq.prototype.diff = function (name) {
    return new Divide(
        this.values.reduce(
            (result, item) =>
                new Add(
                    result,
                    new Multiply(
                        new Multiply(
                            new Const(2),
                            item
                        ),
                        item.diff(name)
                    )
                ),
            new Const(0)
        ),
        new Const(this.values.length)
    );
}

function RMS(...values) {
    Operator.call(
        this,
        "rms",
        (...args) => Math.sqrt(args.reduce((result, item) => result + item * item, 0) / values.length),
        ...values
    );
}

RMS.prototype = Object.create(Operator.prototype);
RMS.prototype.diff = function (name) {
    return new Divide(new Meansq(...this.values).diff(name), new Multiply(new Const(2), this));
}

const OPERATORS = {
    "+": (x, y) => new Add(x, y),
    "-": (x, y) => new Subtract(x, y),
    "*": (x, y) => new Multiply(x, y),
    "/": (x, y) => new Divide(x, y),
    "negate": x => new Negate(x),
    "meansq": (...args) => new Meansq(...args),
    "rms": (...args) => new RMS(...args),
    "sumrec2": (x, y) => new Sumrec2(x, y),
    "sumrec3": (x, y, z) => new Sumrec3(x, y, z),
    "sumrec4": (x, y, z, w) => new Sumrec4(x, y, z, w),
    "sumrec5": (x, y, z, w, v) => new Sumrec5(x, y, z, w, v),
    "hmean2": (x, y) => new HMean2(x, y),
    "hmean3": (x, y, z) => new HMean3(x, y, z),
    "hmean4": (x, y, z, w) => new HMean4(x, y, z, w),
    "hmean5": (x, y, z, w, v) => new HMean5(x, y, z, w, v)
}

function parse(expr) {
    let result = [];
    expr.split(" ").forEach(item => {
        if (item in OPERATORS) {
            let elements = result.splice(-OPERATORS[item].length, OPERATORS[item].length);
            result.push(OPERATORS[item](...elements));
        } else if (item in VARIABLES) {
            result.push(new Variable(item));
        } else if (!isNaN(parseFloat(item))) {
            result.push(new Const(parseFloat(item)));
        }
    });
    return result.pop();
}

class ParsingError extends Error {
    constructor(message) {
        super(message);
    }
}

const DIGITS = "-1234567890.".split("");
const SEPARATORS = "( )".split("");

function parsePrefixPostfix(expr, positionOfOperator = 0) {
    let result = [];
    let counts = [1];
    let brackets = 0;
    let lastPos = -1;
    (expr + " ").split("").forEach((item, index) => {
        if (SEPARATORS.includes(item)) {
            if (index !== 0 && !SEPARATORS.includes(expr[index - 1])) {
                if (counts[counts.length - 1] === 0) {
                    result.push(expr.substring(lastPos + 1, index));
                } else {
                    let value = expr.substring(lastPos + 1, index);
                    if (value in VARIABLES) {
                        result.push(new Variable(value));
                    } else {
                        value.split("").forEach((digit) => {
                            if (!DIGITS.includes(digit)) {
                                throw new ParsingError("Incorrect constant at " + (index + 1));
                            }
                        });
                        result.push(new Const(parseFloat(value)));
                    }
                }
                counts[counts.length - 1]++;
            }
            lastPos = index;
        }

        if (item === '(') {
            brackets++;
            counts.push(0);
        }

        if (item === ')') {
            brackets--;
            let count = counts.pop();
            let operands = result.splice(-count, count);
            let operator = operands.splice(positionOfOperator, 1)[0];
            if (OPERATORS[operator].length !== 0 && OPERATORS[operator].length !== count - 1) {
                throw new ParsingError("Unexpected ')' at " + (index + 1));
            }
            result.push(OPERATORS[operator](...operands));
            counts[counts.length - 1]++;
        }
        if (brackets < 0) {
            throw new ParsingError("Incorrect bracket sequence");
        }
    });
    if (result.length !== 1) {
        throw new ParsingError("Unfinished expression");
    }
    if (brackets !== 0) {
        throw new ParsingError("Incorrect bracket sequence");
    }
    return result.pop();
}

const parsePrefix = (expr) => parsePrefixPostfix(expr, 0);
const parsePostfix = (expr) => parsePrefixPostfix(expr, -1);