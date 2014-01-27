Scala SMT-LIB
=============

Scala SMT-LIB is a generic abstraction over the [SMT-LIB](http://www.smtlib.org/) textual standard.
It enables you to use a typesafe API to build SMT-LIB 2.0 scripts and execute them via any solver
that respects the SMT-LIB standard.

Scala SMT-LIB provides tool support for parsing and printing SMT-LIB syntax. It can help you if you need
to communicate with a native SMT process via its text interface. You can also write Scala wrapper around
SMT solvers and use them from the programmable API. Or you could even get crazier and use a [pure Scala
SMT solver](https://github.com/regb/scabolic) that happens to implement the SMT-LIB api.

The library is still in development and is evolving along with the needs of the projects using it.

Setup
-----

The project is built with sbt. To build the library, just type:

    sbt package

It will produce a jar that you can add to the classpath of your own project.

Examples
--------

The parser can be constructed with a java.io.Reader, you could for example do the following:

    val is = new java.io.FileReader(inputFile)
    val parser = new smtlib.Parser(is)

The parser implements the `Iterator[Command]` and can thus be used in any
interface that expects a `TraversableOnce` element. In particular, assuming an implicit solver
that implements the Interpreter interface is in scope, you can do the following:

    import smtlib.Commands.Script
    smtlib.Interpreter(Script(parser))

API
---

Please refer to the code ;) However, you could start with the Examples section above.

Development
-----------

The project is still evolving and the API will likely go through a couple changes.
It was originally part of [CafeSat](https://github.com/regb/scabolic) and has been made
standalone in order for the [Leon](https://github.com/epfl-lara/leon) project to rely on it
as well. Hopefully, it can be useful to other people as well.

TODO
----

* Expore asynchrous IO: Doesn't seem to make sense with a tree sturcture, and script are short in practice.
* Modularize the background theory definitions as an abstract component that could be extended by third party
  code. Should only provide the Core theory and the basic theories defined by SMT-LIB standard.
