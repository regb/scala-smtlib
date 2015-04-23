CHANGELOG
=========


Current
-------



v0.1
----------------------
*Released 2 April 2015*

Initial version of Scala SmtLib.

* Extensive support for the SMT-LIB language version 2.5
* SMT-LIB expressions represented with a Scala AST
  * Parser turn input stream into AST representation
  * Printer turn AST representation into an SMT-LIB complient string output
* abstraction layer to communicate with solver processes over their textual input 
  * Support for Z3 and CVC4
* Scala SMT-LIB is released under the terms of the MIT license.

