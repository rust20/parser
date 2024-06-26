# Programming Exercise 2: Parser

Remark: this exercise is part of a series, which will continue for the following weeks.

## Language Specification

We will cover a B-like subset of C, with the minor addition of a sized dereference. Some parts of the language are optional for the homework, see below.

    program: function*
    function: ident "(" paramlist? ")" block
    paramlist: ident ("," ident)*
    block: "{" statement* "}"
    statement: ("auto" | "register") ident "=" expr ";"
             | "return" expr? ";"
             | block
             | "while" "(" expr ")" statement
             | "if" "(" expr ")" statement ("else" statement)?
             | expr ";"
    expr: "(" expr ")"
        | ident "(" arglist? ")"
        | ident
        | number
        | "-" expr
        | "!" expr
        | "~" expr
        | "&" expr
        | expr "[" expr sizespec? "]"
        | expr "+" expr
        | expr "-" expr
        | expr "*" expr
        | expr "/" expr
        | expr "%" expr
        | expr "<<" expr
        | expr ">>" expr
        | expr "<" expr
        | expr ">" expr
        | expr "<=" expr
        | expr ">=" expr
        | expr "==" expr
        | expr "!=" expr
        | expr "&" expr
        | expr "|" expr
        | expr "^" expr
        | expr "&&" expr
        | expr "||" expr
        | expr "=" expr
    arglist: expr ("," expr)*
    sizespec: "@" number
    ident: r"[a-zA-Z_][a-zA-Z0-9_]*"
    number: r"[0-9]+"


Additional specifications:

- [x] Comments start with "//" and are ignored until the end of line.
- [x] Whitespace is ignored, except to separate tokens (as in C).
- [x] Multi-char operators like "||" are never treated as "|" "|", unless where is whitespace in between (as in C).
- [x] Keywords are "auto" "register" "if" "else" "while" "return"; they are never identifiers.
- [x] The callee of function calls must be an unparenthesized identifier and never refers to a variable, even if a variable with the same name exists (NOT as in C).
- [x] For identifiers used to refer to variables, the variable must be declared in the current or a parent block; parameters are accessible in the entire function. Variable shadowing in nested scopes is permitted (as in C); identifiers always reference the variable in the most inner scope.
- [x] Operator precedence and associativity:

        14, left-assoc:  []
        13, right-assoc: all unary operators (- ! ~ &)
        12, left-assoc:  * / %
        11, left-assoc:  + -
        10, left-assoc:  << >>
        9,  left-assoc:  < > <= >=
        8,  left-assoc:  == !=
        7,  left-assoc:  &
        6,  left-assoc:  ^
        5,  left-assoc:  |
        4,  left-assoc:  &&
        3,  left-assoc:  ||
        1,  right-assoc: =

- [x] The left-hand side of the assignment operator (`=`) and the operand of the address-of operator (unary `&`) must be a subscript (`a[b]`) or an identifier.
- [x] Variables declared with `register` and parameters are not permitted as operand of the address-of operator (unary "&").
- [x] Valid size specifiers for subscripts are 1, 2, 4, and 8; if omitted, it defaults to 8.
- [x] Defining multiple functions of the same name is an error, as is defining or calling a function with a different number of parameters. (E.g., defining `foo(a)` and calling `foo(1, 2)` is an error.)

Optional operators:
- [x] The following binary operators are optional. While it is suggested to implement them, you do not need to and can also stop implementing them in subsequent homeworks. Operator list: `* / % << >> <= >= & ^ |`

Behavioral specification:

- The signed integer datatype is 64-bit sized, other data types do not exist.
- One byte has 8 bits; the byte order is little endian.
- A subscript interprets the left operand as address and the right operand inside the brackets as offset, which is scaled by the size specifier. I.e., `a[b@X]` refers to the X bytes at address `a + X*b`. When loading from a reduced size (i.e., 1/2/4), as many bytes as specified are loaded from memory and sign-extend to a full integer; stores truncate. Note that therefore `a[b]` is not equivalent to `b[a]` (different than in C).
- Integer overflow is defined as in two's complement (like -fwrapv in GCC).
- Everything else is handled as in ANSI C, including short-circuiting behavior for `&&` and `||`.


## Example program

    gauss(x) {
        register res = -0;
        while (x > 0) {
            res = res + x;
            x = x - 1;
        }
        return res;
    }

    ifTest(x) {
        if (x < -5)
            return;
        x = x + 3;
    }

    isBool(x) { return !!x == x; }

    callTest(a, b) {
        register c = foo(a, b);
        return bar(c, a) + baf(a) + baz(c);
    }

    baz(a, b) { return; }

    unreachableCode(a) {
        if (a > 0) return a;
        else return -a;
        return a + 1;
    }

    foo(a, b) {
        a[b] = b;
        return a[b] + a[b@1];
    }

    addrof(ptr) {
        auto var = 1;
        ptr[1] = &var;
        register ptr2 = &ptr[1];
        ptr2[0] = 2;
        return var;
    }


## Implementation

Implement a parser for the specified language, which creates a full in-memory AST if, and only if, the input program is valid. Dump the AST to standard output for verification. The program must be read from a file (which can also be /dev/stdin), which is specified as path as the first positional argument of the program.

You can write a tokenizer and parser by hand (probably easiest, but needs more code) or use tools like Flex/Bison (probably more effort). Design an AST data structure, which you construct while parsing. (Hint: an AST node might have a fixed or dynamic number of children and perhaps also store an additional number or string.) Verify the semantic soundness (see above) of the AST during or after parsing.

There is no strict limitation on the programming language, but it should support C or C++ bindings (for use with LLVM in subsequent homework), be human-readable, and have minimal dependencies besides compiler and standard library. Also languages like Python are acceptable, but please choose useful/efficient data structures in all cases.


## Analysis (write answers as comment in your code)

Analyze the execution time of your parser and the size of the final AST for a large input program. What is the asymptotic run-time of your parser? Can you find an example, where the total memory footprint of the AST grows super-linear with the input program size?


# Submission

Send an e-mail to engelke+cghomework@in.tum.de until 2023-11-08, 23:59 with:

- Subject: "Homework 2: YourMatrNr YourName"
- A single(!) .tar.xz file attached named with "hw2-YourMatrNr-YourLastName.tar.xz", which contains a single folder "hw2-YourMatrNr-YourLastName", which contains your submission
- The message body can remain empty
- Include a Makefile with compilation directives s.t. `make` compiles the code
- Specify correct dependencies in the Makefile when using parser generators
- Avoid external dependencies and complex build systems (no cmake, cargo, etc.)
- Put the source in a single source file (if easily possible)
- Include answers to theory questions as comments in the source file
