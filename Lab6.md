# Lab 06: Improved Parameters (3.8)

## Introduction

We added support for named and default parameters for funtions and classes. If a value for a parameter with a default value is not given, the compiler completes the default value. Support was also added for explicitly named parameters when calling a function or a cosntructor, allowing parameter reordering.

    fn foo(i: Int, j: Int = 42): Int = { i + j }
    foo(1) // OK, j has default value
    foo(i = 5, j = 7) // OK
    foo(j = 5, i = 7) // OK, can reorder named parameters
    foo(i = 7) // OK
    foo(j = 7) // Wrong, i has no default value
    foo() // Wrong, i has no default value
	
	// Wrong, default parameters have to be at the end
    foo(i: Int = 5, j: Int): Int = { i + j }
    
    // Similarly for case classes
    case class Foo(i: Int, j: Int = 42) extends Bar

## Source

The structure of our extension `src` directory should be as follows:

    src/amyc
     ├── Main.scala
     │
     ├── analyzer   
     │    ├── SymbolTable.scala (updated)
     │    ├── NameAnalyzer.scala (updated)
     │    └── TypeChecker.scala
     │
     ├── ast
     │    ├── Identifier.scala
     │    ├── Printer.scala (updated)
     │    └── TreeModule.scala (updated)
     │
     ├── codegen    
     │    ├── CodeGen.scala
     │    ├── CodePrinter.scala
     │    └── Utils.scala
     │
     ├── interpreter
     │    └── Interpreter.scala
     │
     ├── lib
     │    ├── scallion_3.0.6.jar
     │    └── silex_3.0.6.jar
     │
     ├── parsing
     │    ├── Parser.scala (updated)
     │    ├── Lexer.scala
     │    └── Tokens.scala
     │
     ├── utils
     │    ├── AmycFatalError.scala
     │    ├── Context.scala
     │    ├── Document.scala
     │    ├── Pipeline.scala
     │    ├── Position.scala
     │    ├── Reporter.scala
     │    └── UniqueCounter.scala
     │
     └── wasm        
          ├── Function.scala
          ├── Instructions.scala 
          ├── ModulePrinter.scala
          └── Module.scala

## Extension examples

We have provided multiple extension examples that are runnable and that use the extension.
The structure of our extension examples `extension-examples` directory should be as follows:
	
	extension-examples/ 	(new)
	├── Bank.amy
	├── ClassExample.amy
	├── FunctionExample.amy
	├── List.amy
	├── Person.amy
	├── Rational.amy
	└── TestLists.amy

You can run these examples with the following commands  in the sbt terminal :
-  Bank : `run library/Std.amy extension-examples/Bank.amy`
-  ClassExample : `run library/Std.amy extension-examples/ClassExample.amy`
-  FunctionExample : `run library/Std.amy extension-examples/FunctionExample.amy`
-  Person : `run library/Std.amy extension-examples/Person.amy`
-  Rational : `run library/Std.amy extension-examples/Rational.amy`
-  TestLists : `run library/Std.amy extension-examples/List.amy library/Option.amy extension-examples/TestLists.amy`

