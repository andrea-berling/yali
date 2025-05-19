# YALI: Yet Another Lox Interpreter

This is my take on the ["Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview) by [CodeCrafters](https://codecrafters.io). It's a simple tree walk interpreter for the [Lox programming language](https://craftinginterpreters.com/the-lox-language.html), a small, dynamically typed programming languages with support for classes, inheritance, functions as first class values, closures, and a risibly small standard library (my version only contains the `clock` function to get a Unix timestamp). Mostly a didactic project, it gave me the chance to get my hands dirty with Rust and with things like tokenizing, parsing text into ASTs, evaluating expressions, resolving scopes, implementing dynamic dispatch, and more. I plan to expand on this project in the future to give the user the possibility to compile the Lox code to machine code. In the process, I will also add a Garbage Collector, support for references/pointers to other objects, and probably expand the standard library.

All you need to do to get started with the interpreter, is building it:

```bash
cargo build -r
```

## Command line interface

To tokenize a program and see a list of the scanned tokens, use the following command:

```bash
yali tokenize my-program.lox
```

Example:

`my-program.lox`

```lox
print "quz" + "world" + "baz";
print 78 - 96;
print "baz" == "hello";
```

Result:

```
PRINT print null
STRING "quz" quz
PLUS + null
STRING "world" world
PLUS + null
STRING "baz" baz
SEMICOLON ; null
PRINT print null
NUMBER 78 78.0
MINUS - null
NUMBER 96 96.0
SEMICOLON ; null
PRINT print null
STRING "baz" baz
EQUAL_EQUAL == null
STRING "hello" hello
SEMICOLON ; null
EOF  null
```

To parse a program and see a textual representation of the parse tree, use the following command:

```bash
yali parse my-program.lox
```

Result:

```
{print (+ (+ quz world) baz); print (- 78.0 96.0); print (== baz hello)}
```


To evaluate an expresion and print the result, use the following command:

```bash
yali evaluate my-program.lox
```

Example:

`my-program.lox`
```
"quz" + "world" + "baz"
```

Result:

```
quzworldbaz
```

To run a full Lox program, use the following command:

```bash
yali run my-program.lox
```

Example:

`my-program.lox`
```
class Animal {
  speak() {
    return "Animal speaks";
  }
  makeSound() {
    return "Generic sound";
  }
  communicate() {
    return this.speak() + " : " + this.makeSound();
  }
}

class Dog < Animal {
  speak() {
    return "Dog speaks";
  }

  makeSound() {
    return "Woof";
  }
}

class Puppy < Dog {
  speak() {
    return "Puppy speaks";
  }
}

var animal = Animal();
var dog = Dog();
var puppy = Puppy();

print animal.communicate();
print dog.communicate();
print puppy.communicate();
```

Result:

```
Animal speaks : Generic sound
Dog speaks : Woof
Puppy speaks : Woof
```

## Testing

This project comes with a full snapshot test suite made of programs or inputs for the interpreter and expected outputs, errors, and exit status, for various components of the interpreter (i.e. the tokenizer, the parser, the evaluator, and the interpreter). To run it, use the following command:

```bash
cargo test
```

You can also run the test suite for a single interpreter component:

```bash
cargo test parser
```
