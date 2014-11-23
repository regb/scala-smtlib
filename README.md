Scala SMT-LIB
=============

Scala SMT-LIB is a lightweight abstraction over the
[SMT-LIB](http://www.smtlib.org/) standard.  It enables you to use a typesafe
API in Scala to build SMT-LIB 2.0 scripts and execute them via any solver that
supports the SMT-LIB standard.

Scala SMT-LIB provides the tools for parsing and printing the SMT-LIB syntax.
It can help you if you need to communicate with a native SMT process via its
text interface. You can also write a Scala wrapper around an SMT solver and use
it from a friendlier API. You could even get crazier and use a [pure Scala SMT
solver](https://github.com/regb/scabolic) that happens to implement the SMT-LIB
api.

The library is still in development and is evolving along with the needs of the
projects using it.

Motivation
----------

Scala SMT-LIB deals with the messy details of parsing and writing SMT-LIB. It
provides a programmer friendly interface to simplify the development of tools
that rely on SMT-LIB.

You may want to use Scala SMT-LIB if:
* You plan to write a tool that will take as input some SMT-LIB files, but do
  not wish to spend too much time figuring out the SMT-LIB standard. Then you can
  plug-in the parser component of Scala SMT-LIB in your tool and only deal with
  the Scala representation of the few SMT-LIB commands of your interest.
* You need to output a complex SMT-LIB encoding of some mathematical problems. We got
  you covered: You can programatically build a set of expressions using the
  Scala SMT-LIB abstract syntax tree, and use the printer components to get
  a valid SMT-LIB representation to pass along to another tool.
* You need to query an external black-box SMT solver, but the task of setting
  up a proper communication with this strange beast seems a bit too daunting? 
  Scala SMT-LIB offers a module that abstracts SMTLIB-compliant solvers. You can 
  program your tool against this simple high-level API. Scala SMT-LIB provides 
  integration with Z3 and CVC4 out of the box, and you can add support for 
  any other solver by implementing a relatively thin interface.
   

Setup
-----

The project is built with sbt. To build the library, just type:

    sbt package

It will produce a jar that you can add to the classpath of your own project.

If you are building your project using sbt, it is possible to setup a reference
to this github repository in the build system to automatically get the most
recent build. If you are interested in this route, you should check the sbt
official documentation.

Examples
--------

To get a parser instance, you need to provide a java.io.Reader and a lexer:

    val is = new java.io.FileReader(inputFile)
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

The parser then provides a `parseCommand` functions that will consume the input
until the end of the next command. It returns `null` when the end of file is
reached.


API
---

Please refer to the code ;) However, you could start with the Examples section
above.

Important files are /src/main/scala/smtlib/parser/Commands.scala and
/src/main/scala/smtlib/parser/Terms.scala for the abstract syntax tree of SMT-LIB.
The lexer directory provides low level parsing of tokens, then the parser provides
the extraction of commands. The printer helps with printing out SMT-LIB standard.
Finally the theories module provides tree builders to create theory-specific formulas.


Development
-----------

The project is still under development and the API will likely go through quite a few
changes. It was originally part of [CafeSat](https://github.com/regb/scabolic)
and has been made standalone in order for the
[Leon](https://github.com/epfl-lara/leon) project to rely on it.
Hopefully, it can be useful to other people as well.

Road Map
--------

A list of potential features to be added.

* Type checking of the input script: Make sure the logic declaration corresponds to the syntax of the formulas,
  check correct syntax of terms and proper declaration of all uninterpreted symbols used.
* Expore asynchrous IO: Doesn't seem to make sense with a tree sturcture, and script are short in practice.
* Modularize the background theory definitions as an abstract component that could be extended by third party
  code. Should only provide the Core theory and the basic theories defined by SMT-LIB standard.
