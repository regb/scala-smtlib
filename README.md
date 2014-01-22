#Scala SMT-LIB

SMT-LIB library for Scala. It provides full support for parsing and printing SMT-LIB2 syntax.

##Setup

The project is built with sbt. To build the library, just type:

    sbt package

It will produce a jar that you can add to the classpath of your own project.

##Examples

##API

Please refer to the code ;) However, you could start with the Examples section above.

##Development

The project is still evolving and the API will likely go through a couple changes.
It was originally part of [CafeSat](https://github.com/regb/scabolic) and has been made
standalone in order for the [Leon](https://github.com/epfl-lara/leon) project to rely on it
as well. Hopefully, it can be useful to other people as well.

##TODO

* Expore asynchrous IO: Doesn't seem to make sense with a tree sturcture, and script are short in practice.
* Modularize the background theory definitions as an abstract component that could be extended by third party
  code. Should only provide the Core theory and the basic theories defined by SMT-LIB standard.
