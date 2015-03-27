Scala SMT-LIB
=============

Scala SMT-LIB is a lightweight, no dependency, abstraction over the
[SMT-LIB](http://www.smtlib.org/) standard for Satisfiability Modulo Theory
(SMT) solvers. It lets you use a type safe Scala API to build SMT-LIB scripts
and ship them to any SMT-LIB complient solver over a process interface.

Scala SMT-LIB provides the tools for parsing and printing the SMT-LIB syntax.
It can help you if you need to communicate with a native SMT solver process via its
text interface. You can also write a Scala wrapper around an SMT solver and use
it from a friendlier API. You could even get crazier and use a [pure Scala SMT
solver](https://github.com/regb/scabolic) that happens to implement the SMT-LIB
api.

The library is still under active development and is evolving along with the
needs of the projects using it. Essentially it means the API is going to change
quite frequently until we are satisfied with how it feels. However, the code is
actually quite robust, and comes with an extensive test suite. You should not fear
relying on a current snapshot.

Applications
------------

Scala SMT-LIB deals with the messy details of parsing and writing SMT-LIB. It
provides a programmer friendly interface to simplify the development of tools
that rely on SMT-LIB.

You may want to use Scala SMT-LIB if:
* You plan to write a tool that will use SMT-LIB as its input format, but do
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

The project is built with [sbt](http://www.scala-sbt.org/). To build the
library, just type:

    sbt package

It will produce a jar that you can add to the classpath of your own project.

If you are building your project using sbt, it is possible to setup a reference
to this github repository in the build system to automatically get the most
recent build. [Here](https://github.com/regb/cafesat/blob/master/build.sbt) is
an example of how to do it, you can pick any commit. If you are interested in
this route, you should check the sbt official documentation.

Getting started with Examples
-----------------------------

To construct a parser, you will need a java.io.Reader and a lexer:

    val is = new java.io.FileReader("INPUT")
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

The parser then provides a `parseCommand` functions that will consume the input
until the end of command:

    val script: List[Command] = {
      var cmds = new ListBuffer[Command]
      var cmd = parser.parseCommand
      while(cmd != null)
        cmds.append(cmd)
      cmds.toList
    }

`parseCommand` returns `null` when the end of file is reached.

You can decompose a command using pattern matching:

    cmd match {
      case Assert(term) => ???
      case CheckSat() => ???
      case Pop(1) => ???
    }

Low Level API
-------------

This section describes the low level API of Scala SMT-LIB. In the future, it is
expected to be wrapped by a higher-level API to perform common operations.

The [`lexer`](/src/main/scala/smtlib/lexer) package implements low level
parsing of [`Tokens`](/src/main/scala/smtlib/lexer/Tokens.scala),


then the [`parser`](/src/main/scala/smtlib/parser) provides the extraction of
commands. 

A `Parser` uses exceptions to signal parsing error, they are defined in the
[`Parser` companion object](/src/main/scala/smtlib/parser/Parser.scala). They
provide a bunch of information, including the token on which the parsing
failed, the expected tokens, and the position in the input (the position is an
attribute of the token and can be accessed through it).

A `Command` is the root of a complex abstract syntax tree (AST) representing
the corresponding SMT-LIB command. It is defined
[here](/src/main/scala/smtlib/parser/Commands.scala). A few commands take as
parameter a `Term`, whose AST is defined
[here](/src/main/scala/smtlib/parser/Terms.scala).

The [`printer`](/src/main/scala/smtlib/printer) helps with printing out SMT-LIB
complient commands.  This means that the output of a printer can be send
directly to an SMT solver.

Finally the [`theories`](/src/main/scala/smtlib/theories) module provides tree
builders to create theory-specific formulas.  Each theory module provides
`apply` and `unapply` methods on various object to manipulate the `Term`
representing the actual theory expression.

Development
-----------

The project is still under development and the API will likely go through quite a few
changes. It was originally part of [CafeSat](https://github.com/regb/scabolic)
and has been made standalone in order for the
[Leon](https://github.com/epfl-lara/leon) project to rely on it.
Hopefully, it can be useful to other people as well.

Changelog
---------

###Current

This is the first release.
