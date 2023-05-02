const cnst = value => () => value;
const one = cnst(1);
const two = cnst(2);
const CONSTS = {"one": one, "two": two}

const VARIABLES = {"x": 0, "y": 1, "z": 2}
const variable = name => (...args) => {
    return args[VARIABLES[name]];
}
const operator = f => (...args) => (x, y, z) => {
    let elements = [];
    args.forEach((item) => elements.push(item(x, y, z)));
    return f(...elements);
};

const argFind = (comp) => operator((...args) => {
    return args.reduce((result, item, index, arr) => comp(item, arr[result])? index : result, 0);
})

const argMin = argFind((x, y) => x < y);
const argMax = argFind((x, y) => x > y);

const argMin3 = (x, y, z) => argMin(x, y ,z);
const argMax3 = (x, y, z) => argMax(x, y ,z);
const argMin5 = (x, y, z, w, v) => argMin(x, y, z, w, v);
const argMax5 = (x, y, z, w, v) => argMax(x, y, z, w, v);

const madd = operator((x, y, z) => x * y + z);
const floor = operator(x => Math.floor(x));
const ceil = operator(x => Math.ceil(x));
const add = operator((x, y) => x + y);
const subtract = operator((x, y) => x - y);
const multiply = operator((x, y) => x * y);
const divide = operator((x, y) => x / y);
const negate = operator(x => -x);

const OPERATORS = {
    "negate": negate,
    "^": ceil,
    "_": floor,
    "*+": madd,
    "+": add,
    "-": subtract,
    "*": multiply,
    "/": divide,
    "argMin3": argMin3,
    "argMax3": argMax3,
    "argMin5": argMin5,
    "argMax5": argMax5
}
const OPERATORS_ARGUMENT_COUNT = {
    "negate": 1,
    "^": 1,
    "_": 1,
    "*+": 3,
    "+": 2,
    "-": 2,
    "*": 2,
    "/": 2,
    "argMin3": 3,
    "argMax3": 3,
    "argMin5": 5,
    "argMax5": 5
}
const parse = (expr) => {
    let result = [];
    expr.split(" ").forEach(item => {
        if (item in OPERATORS) {
            let elements = result.splice(-OPERATORS_ARGUMENT_COUNT[item], OPERATORS_ARGUMENT_COUNT[item]);
            result.push(OPERATORS[item](...elements));
        } else if (item in VARIABLES) {
            result.push(variable(item));
        } else if (item in CONSTS) {
            result.push(CONSTS[item]);
        } else if (!isNaN(parseFloat(item))) {
            result.push(cnst(parseFloat(item)));
        }
    });
    return result.pop();
}

let ex = add(
    subtract(
        multiply(
            variable("x"),
            variable("x")
        ),
        multiply(
            cnst(2),
            variable("x")
        )
    ),
    cnst(1)
);

for (let i = 0; i <= 10; i++) {
    console.log(ex(i, 0, 0));
}