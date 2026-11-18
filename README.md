Scala SMT-LIB [![scala-smtlib CI](https://github.com/epfl-lara/scala-smtlib/actions/workflows/scala-smtlib-CI.yml/badge.svg)](https://github.com/epfl-lara/scala-smtlib/actions/workflows/scala-smtlib-CI.yml)
=============

Scala SMT-LIB is a lightweight, no dependency, abstraction over the
[SMT-LIB](http://www.smtlib.org/) standard for Satisfiability Modulo Theory
(SMT) solvers. It lets you use a type safe Scala API to build SMT-LIB scripts
and ship them to any SMT-LIB complient solver over a process interface.

Scala SMT-LIB provides the tools for parsing and printing the SMT-LIB syntax.
It can help you if you need to communicate with a native SMT solver process via its
text interface. You can also write a Scala wrapper around an SMT solver and use
it with a friendlier API. You could even get crazier and use a [pure Scala SMT
solver](https://github.com/regb/cafesat) that happens to implement the SMT-LIB
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
  Scala SMT-LIB offers a module that abstracts SMTLIB-compliant solvers. You
  can program your tool against this simple high-level API. Scala SMT-LIB
  provides integration with [Z3](https://github.com/Z3Prover/z3) and
  [CVC4](http://cvc4.cs.nyu.edu/web/) out of the box, and you can add support
  for any other solver by implementing a relatively thin interface.
   

Setup
-----

To include Scala SMT-LIB in your project, you can add it as a dependency
referring to a commit hash. You can add the following to your `build.sbt`:

```scala
def remoteProject(repo: String, version: String) = RootProject(uri(s"${repo}#${version}"))

// Choose a commit hash to pin the Scala SMT-LIB version (replace with the latest or chosen commit):
val smtlibRepository = "https://github.com/epfl-lara/scala-smtlib.git"
val smtlibCommitHash = "a8fc084fa48a59f0f58b6f3fba5433b8fe5eb280" // example commit

lazy val scalaSmtlib = remoteProject(smtlibRepository, smtlibCommitHash)

// And depend on it in your project
lazy val yourProject = (project in file("."))
  .settings(
    // ...
  )
  .dependsOn(scalaSmtlib) // <<<
```

Releases for Scala 3 are published on Maven Central, under `ch.epfl.lara`. They are compiled
with Scala 3.9, and work with Scala 3.9 and later. Add the following to your project settings in
`build.sbt`, with the latest version:

```scala
libraryDependencies += "ch.epfl.lara" %% "scala-smtlib" % "<version>"
```

The last stable release for Scala 2.11 is available on Sonatype, add the
following to your project settings in `build.sbt`:

```scala
libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2"
```

Getting Started with Examples
-----------------------------

To construct a parser, you will need a `java.io.Reader` and a lexer:

```scala
val is = new java.io.FileReader("INPUT")
val lexer = new smtlib.lexer.Lexer(is)
val parser = new smtlib.parser.Parser(lexer)
```

The parser then provides a `parseCommand` functions that will consume the input
until the end of command:

```scala
val script: List[Command] = {
  var cmds = new ListBuffer[Command]
  var cmd = parser.parseCommand
  while(cmd != null)
    cmds.append(cmd)
  cmds.toList
}
```

`parseCommand` returns `null` when the end of file is reached.

You can decompose a command using pattern matching:

```scala
import smtlib.parser.Commands._
cmd match {
  case Assert(term) => ???
  case CheckSat() => ???
  case Pop(1) => ???
}
```

If you want to generate a script to feed to a solver, you can build the AST
explicitly, and use the pretty printers:

```scala
import smtlib.parser.Commands._
import smtlib.parser.theories.Ints._
val x = QualifiedIdentifier(SimpleIdentifier(SSymbol("x")))
val y = QualifiedIdentifier(SimpleIdentifier(SSymbol("y")))
val formula = Assert(LessThan(NumeralLit(0), Plus(x, y)))
smtlib.printer.RecursivePrinter.toString(formula) //(assert (< 0 (+ x y)))
```
    
The above is a bit verbose due to the objective of supporting all of SMT-LIB. We
are hoping to provide a nicer API in the future to build SMT-LIB scripts, at
least in the common cases, but the AST will probably remain at the core of the
library.

Low Level API
-------------

This section introduces the low level API of Scala SMT-LIB. In the future, it is
expected to be wrapped by a higher-level API to perform most common operations.
However, that API is expected to remain relatively stable and at the core of the
library, so if that fits your needs you should be able to safely use it. It comes
with a relatively extensive test suite to make sure it is working as expected.


### Lexing

The [`lexer`](/src/main/scala/smtlib/lexer) package implements low level
parsing of [`Tokens`](/src/main/scala/smtlib/lexer/Tokens.scala), The 
[`Lexer`](/src/main/scala/smtlib/lexer/Lexer.scala) lexes the input into
tokens. It reads lazily from a `java.io.Reader`, on invocation of the `nextToken`
method:

```scala
class Lexer(reader: java.io.Reader) {
  def nextToken: Token = { ... }
}
```

One call to `nextToken` will only consume the input until the end of the
current token. It will read one character at a time from the input `Reader`
until it was able to unambiguously identify the end of the token. You can
provide a buffered input as a way to improve performance on some use cases.

`nextToken` either returns a valid `Token`, or `null` if EOF is reached in the
input `Reader`. It will signal errors with exceptions: a value of `null` is not
an error and is expected to be eventually reached (unless the input never ends,
such as `stdin`). The following two exceptions can be thrown by the lexer:

* `UnexpectedCharException` signals that an error in the input. It corresponds
  to a malformed token --- for example, a `#ffff` (instead of `#xffff`) would
  throw the exception on the first `f`. The exception indicates the position
  in the input.
* `UnexpectedEOFException` occurs when the EOF is reached in the middle of an
  incomplete token. For example, a string literal which is not closed before the
  EOF.


### Parsing

Usually one does not need to work on a token by token basis, and is only
interested in fully formatted SMT-LIB expressions. The
[`parser`](/src/main/scala/smtlib/parser) provides the extraction of SMT-LIB
expressions. The [`Parser`](/src/main/scala/smtlib/parser/Parser.scala) consumes
tokens generated by the above lexer:

```scala
class Parser(lexer: smtlib.lexer.Lexer) {
  def nextCommand: Command = { ... }
  def nextTerm: Term = { ... }
}
```

It provides many `nextX` methods (more than shown above) to parse any expressions
defined by the SMT-LIB standard grammar. The more common are the `nextCommand`
and the `nextTerm` to parse respectively a command and a term-formula expression.

A `Parser` uses exceptions to signal parsing error, they are defined in the
[`Parser` companion object](/src/main/scala/smtlib/parser/Parser.scala). They
provide a bunch of information, including the token on which the parsing
failed, the expected tokens, and the position in the input (the position is an
attribute of the token and can be accessed through it).

A `Command` is the root of a complex abstract syntax tree (AST) representing
the corresponding SMT-LIB command. A few commands take as
parameter a `Term`. Their ASTs are defined
[here](/src/main/scala/smtlib/trees/Trees.scala).

### Printing

The [`printer`](/src/main/scala/smtlib/printer) helps with printing out SMT-LIB
compliant commands. This means that the output of a printer can be sent
directly to an SMT solver.

### Standard Theories

Finally, the [`theories`](/src/main/scala/smtlib/theories) module provides tree
builders to create theory-specific formulas. Each theory module provides `apply`
and `unapply` methods on various object to manipulate the `Term` representing
the actual theory expression.

Development
-----------

The library is still under development and the API will likely go through quite
a few changes. It was originally part of
[CafeSat](https://github.com/regb/cafesat) and has been made standalone in order
for the [Inox](https://github.com/epfl-lara/inox) and
[Stainless](https://github.com/epfl-lara/stainless) projects to rely on it.
Hopefully, it can be useful to other people as well.

### Testing

In order to attain a decent level of quality, there is a relatively strict
policy for testing. Testing is separated in two levels of testing: unit tests
and integration tests. Unit test are dependency free and run entirely in memory.
While integration tests will rely on things like file system and external
solvers. Unit test are very fast and should be easy to run as part of a regular
build cycle (you can run them after each compile), while integration tests are a
bit slower and are meant to pass on each commit.

In SBT, the command `sbt test` will run the unit test suite, while the command
`sbt IntegrationTest/test` will run the integration test suite. During
development, it should be fine to run in [quick-testing mode](https://www.scala-sbt.org/1.x/docs/Testing.html#testQuick), as `sbt ~testQuick`, running only previously failing tests or those expected to change on file save.

### Building the Sources

The project is built with [sbt](http://www.scala-sbt.org/). To build the
library, just type:

```console
$ sbt package
```

It will produce a jar that you can add to the classpath of your own project.

Optionally, you can test Scala SMT-LIB in your environment by running the test
suite. The tests are organized in unit and functional tests. Tu run the unit
tests (very fast) you can type:

```console
$ sbt test
```

The integration tests test end-to-end flows of Scala SMT-LIB. They do take
a bit more time and require some setup in your environment. In particular, they
will try to use the SMT solvers `z3`, `cvc4`, `cvc5`, and `bitwuzla` if they are
available in your PATH (the commands tried are exactly the solver names as
listed). If present in the PATH, Scala SMT-LIB will test its interpreter module
directly against these SMT solvers. 

The solvers binaries can also be obtained through the
[`scripts/install_solvers.sh`](scripts/install_solvers.sh) script. You can run
it with:

```console
$ INSTALL_SOLVERS=1 scripts/install_solvers.sh
```
  
Default known versions of the solvers are used in the script unless given on the
command line.

You can run those tests with:

```console
$ sbt IntegrationTest/test
```

Releasing
---------

Releases are published to Maven Central by the
[Release workflow](.github/workflows/release.yml). Run it from the Actions tab ("Run workflow" on
`master`), or with `gh workflow run release.yml -f version=v0.3.0`, giving the version with a
leading `v`. It tags the current commit (the build takes its version from the tag, through
`git describe`), tests, signs and publishes that version, then pushes the tag and creates the
GitHub release, with generated notes and the JAR attached. A version can't be published twice.

To check a release before it goes out, choose the mode `upload`: it only uploads the artifacts to
the [Central Portal](https://central.sonatype.com/publishing/deployments), to publish or drop
them there, and pushes no tag.

Locally, `sbt publishSigned` signs the artifacts and stages them in `target/sona-staging`.

Changelog
---------

See [Changelog](/CHANGELOG.md).
