package smtlib
package theories
package experimental

import parser.Terms._

object Strings {

  /** The strings and format are taken from here:
    * http://cvc4.cs.nyu.edu/wiki/Strings#Examples
    */

  private val StringConcat    = "str.++"
  private val StringLength    = "str.len"
  private val StringAt        = "str.at"
  private val StringSubstring = "str.substr"
  private val StringInRegex   = "str.in.re"
  private val StringToRegex   = "str.to.re"
  
  private val StringContains    = "str.contains"
  private val StringIndexOf     = "str.indexof"
  private val StringReplace     = "str.replace"
  private val StringPrefixOf    = "str.prefixof"
  private val StringSuffixOf    = "str.suffixof"
  private val StringStringToInt = "str.to.int"
  private val StringIntToString = "int.to.str"
  
  private val RegexConcat      = "re.++"
  private val RegexUnion       = "re.union"
  private val RegexInter       = "re.inter"
  private val RegexKleeneStar  = "re.*"
  private val RegexKleeneCross = "re.+"
  private val RegexKleeneOpt   = "re.opt"
  private val RegexRange       = "re.range"
  private val RegexLoop        = "re.loop"
  private val RegexLoop2       = "re.loop2"
  private val RegexEmpty       = "re.nostr"
  private val RegexAllChar     = "re.allchar"
  
  
  object StringSort {
    def apply(): Sort = {
      Sort(Identifier(SSymbol("String")))
    }
    def unapply(sort: Sort): Boolean = sort match {
      case Sort(Identifier(SSymbol("String"), Seq()), Seq()) => true
      case _ => false
    }
  }

  object StringLit {
    def apply(value: String): Term = SString(value)
    def unapply(term: Term): Option[String] = term match {
      case SString(value) => Some(value)
      case _ => None
    }
  }
  
  object Length {
    def apply(t: Term): Term = 
      FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(StringLength))), Seq(t))
    def unapply(term: Term): Option[Term] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(StringLength), Seq()),
          None
        ), Seq(t)) => Some(t)
      case _ => None
    }
  }
  
  /** Terms requiring at least two arguments */
  private[experimental] class ConcatLike(NodeSymbol: String) {
    def apply(t: Seq[Term]): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(NodeSymbol))),
        t
      )
    def apply(t1: Term, t2: Term, t: Term*): Term = apply(Seq(t1, t2) ++ t)
    def unapplySeq(term: Term): Option[Seq[Term]] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(NodeSymbol), Seq()),
          None
        ), seqTerm) if seqTerm.length >= 2 => Some(seqTerm)
      case _ => None
    }
  }
  
  /** Functions with only one variable. */
  private[experimental] class ZeroVar(NodeSymbol: String) {
    def apply(): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(NodeSymbol))),
        Seq()
      )
    def unapply(term: Term): Option[Unit] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(NodeSymbol), Seq()),
          None
        ), Seq()) => Some(())
      case _ => None
    }
  }
  
  /** Functions with only one variable. */
  private[experimental] class OneVar(NodeSymbol: String) {
    def apply(t1: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(NodeSymbol))),
        Seq(t1)
      )
    def unapply(term: Term): Option[Term] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(NodeSymbol), Seq()),
          None
        ), Seq(t1)) => Some(t1)
      case _ => None
    }
  }
  
  /** Terms with exactly two arguments */
  private[experimental] class TwoVars(NodeSymbol: String) {
    def apply(t1: Term, t2: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(NodeSymbol))),
        Seq(t1, t2)
      )
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(NodeSymbol), Seq()),
          None
        ), Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }
  }
  
  /** Terms with exactly three arguments */
  private[experimental] class ThreeVars(NodeSymbol: String) {
    def apply(t1: Term, t2: Term, t3: Term): Term =
      FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol(NodeSymbol))),
        Seq(t1, t2, t3)
      )
    def unapply(term: Term): Option[(Term, Term, Term)] = term match {
      case FunctionApplication(
        QualifiedIdentifier(
          Identifier(SSymbol(NodeSymbol), Seq()),
          None
        ), Seq(t1, t2, t3)) => Some((t1, t2, t3))
      case _ => None
    }
  }
  
  /** String concatenation takes at least 2 arguments. */
  object Concat extends ConcatLike(StringConcat)
  
  /** Character in String. First argument is a string term and second is a natural number. The index is starting from 0. */
  object At extends TwoVars(StringAt)
  
  /** Substring given string, start and length/offset */
  object Substring extends ThreeVars(StringSubstring)
  
  /** Membership Constraint where first arg is a string term and second a regular expression. */
  object InRegex extends TwoVars(StringInRegex)
  
  /** String to Regular Expression Conversion.
    * The statement turns a regular expression that only contains a string s.
    */
  object ToRegex extends OneVar(StringToRegex)
  
  object Regex {
    /** Membership constraint. See [InRegex]. */
    val In = InRegex
    /** Membership constraint. See [ToRegex]. */
    val To = ToRegex
    
    lazy val Star   = KleeneStar
    lazy val *      = KleeneStar
    lazy val Cross  = KleeneCross
    lazy val Plus   = KleeneCross
    lazy val +      = KleeneCross
    lazy val ?      = Opt
    lazy val NoStr  = Empty
    
    /** Regular Expression Concatenation. */
    object Concat extends ConcatLike(RegexConcat)
    /** Regular Expression Alternation. */
    object Union extends ConcatLike(RegexUnion)
    /** Regular Expression Intersection. */
    object Inter extends ConcatLike(RegexInter)
    /** Regular Expression Kleene-Star (equivalent to Loop(r, 0)) */
    object KleeneStar extends OneVar(RegexKleeneStar)
    /** Regular Expression Kleene-Cross (equivalent to Loop(r, 1)) */
    object KleeneCross extends OneVar(RegexKleeneCross)
    /** Regular Expression Option marker (equivalent to Loop(r, 0, 1)) */
    object Opt extends OneVar(RegexKleeneOpt)
    /** Regular Expression Range where arguments s, t are single characters in double quotes, e.g. "a", "b". It returns a regular expression that contains any character between s and t.*/
    object Range extends TwoVars(RegexRange)
    
    /** Regular Expression Loop with arguments (r, l, u) where r is a regular expression, l is a non-negative constant integer, and u is an optional non-negative constant integer. It returns a regular expression that contains at least l repetitions of r and at most u repetitions of r. If l >= u, it returns exactly l repetitions of r.*/
    object Loop {
      def apply(r: Term, minRepetitions: Term, maxRepetitions: Term): Term =
        FunctionApplication(
          QualifiedIdentifier(Identifier(SSymbol(RegexLoop))),
          Seq(r, minRepetitions, maxRepetitions)
        )
      def apply(r: Term, minRepetitions: Term): Term =
        FunctionApplication(
          QualifiedIdentifier(Identifier(SSymbol(RegexLoop))),
          Seq(r, minRepetitions)
        )
      def unapplySeq(term: Term): Option[Seq[Term]] = term match {
        case FunctionApplication(
          QualifiedIdentifier(
            Identifier(SSymbol(RegexRange), Seq()),
            None
          ), seqTerm) if seqTerm.length == 2 || seqTerm.length == 3 => Some(seqTerm)
        case _ => None
      }
    }
    
    /** Empty Regular Expression */
    object Empty extends ZeroVar(RegexEmpty)
    
    /** All characters Regular Expression */
    object AllChar extends ZeroVar(RegexAllChar)
  }

  /**
    Following functions are under the --strings-exp option. They are under active refinement. Once they are stable, we will move them to the default mode. Please let us know when you have some suggestions.
  */
  object Experimental {
    /** String Contain. Arguments (s,t) where s and t are string terms. It returns true if the string s contains the string t. This function determines whether the string t can be found within the string s, returning true or false as appropriate. */
    object Contains extends TwoVars(StringContains)
    
    /** String IndexOf. Arguments (s, t, i) where s is a string, t is a non-empty string and i is a non-negative integer. This function returns the position of the first occurrence of the specified value t in the string s after the index i. It returns -1 if the value to search for never occurs. */
    object IndexOf extends ThreeVars(StringIndexOf)
     
    /** String Replacement. Arguments (s, t1, t2) where s, t1 and t2 are string terms, t1 is non-empty. This function searches the string s for the specified value t1, and returns a new string where the first occurrence of the specified value t1 is replaced by the string t2. */
    object Replace extends ThreeVars(StringReplace)
    
    /** String PrefixOf. Arguments (s, t) where s and t are string terms. It returns true if the string s is a prefix of the string t. */
    object PrefixOf extends TwoVars(StringPrefixOf)
    
    /** String SuffixOf. Arguments (s, t) where s and t are string terms. It returns true if the string s is a suffix of the string t. */
    object SuffixOf extends TwoVars(StringSuffixOf)
    
    /** String To Integer Conversion. Argument s where s is a string term. It returns the corresponding natural number if s is valid; otherwise, it returns -1. */
    object StringToInt extends OneVar(StringStringToInt)
    
    /** Integer To String Conversion. Argument s where i is an integer term. It returns the corresponding string if i is a natural number; otherwise, it returns an empty string. */
    object IntToString extends OneVar(StringIntToString)
  }
}
