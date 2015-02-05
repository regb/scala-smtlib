package smtlib
package common

import org.scalatest.FunSuite

class HexadecimalTests extends FunSuite {

  test("Build hexadecimal with one digit string") {
    val zero = Hexadecimal.fromString("0")
    assert(zero !== None)
    zero.foreach(zero => assert(zero.repr === "0"))
    
    val one = Hexadecimal.fromString("1")
    assert(one !== None)
    one.foreach(one => assert(one.repr === "1"))
    
    val ten = Hexadecimal.fromString("A")
    assert(ten !== None)
    ten.foreach(ten => assert(ten.repr === "A"))
  }

  test("Build hexadecimal with strings of multiple digits") {
    val hexa1 = Hexadecimal.fromString("12AB")
    assert(hexa1 !== None)
    hexa1.foreach(hexa1 => assert(hexa1.repr === "12AB"))

    val hexa2 = Hexadecimal.fromString("00F2")
    assert(hexa2 !== None)
    hexa2.foreach(hexa2 => assert(hexa2.repr === "00F2"))
  }

  test("Build hexadecimal with lower caps is represented as upper caps") {
    val hexa1 = Hexadecimal.fromString("a")
    assert(hexa1 !== None)
    hexa1.foreach(hexa1 => assert(hexa1.repr === "A"))

    val hexa2 = Hexadecimal.fromString("00fa")
    assert(hexa2 !== None)
    hexa2.foreach(hexa2 => assert(hexa2.repr === "00FA"))
  }

  test("Build hexadecimal from an invalid string returns None") {
    val hexa1 = Hexadecimal.fromString("g")
    assert(hexa1 === None)

    val hexa2 = Hexadecimal.fromString("0g")
    assert(hexa2 === None)

    val hexa3 = Hexadecimal.fromString("g001")
    assert(hexa3 === None)
  }


  test("toInt returns correct value for small positive numbers") {
    assert(Hexadecimal.fromString("0").get.toInt === 0)
    assert(Hexadecimal.fromString("1").get.toInt === 1)
    assert(Hexadecimal.fromString("a").get.toInt === 10)
    assert(Hexadecimal.fromString("f").get.toInt === 15)
    assert(Hexadecimal.fromString("10").get.toInt === 16)
    assert(Hexadecimal.fromString("1a").get.toInt === 26)
    assert(Hexadecimal.fromString("001a").get.toInt === 26)
    assert(Hexadecimal.fromString("3a").get.toInt === 58)
  }

  test("toInt returns correct value for negative numbers") {
    assert(Hexadecimal.fromString("FFFFFFFF").get.toInt === -1)
    assert(Hexadecimal.fromString("FFFFFFFE").get.toInt === -2)
    assert(Hexadecimal.fromString("80000000").get.toInt === -2147483648)
  }

  test("fromInt converts positive int to hexadecimal") {
    assert(Hexadecimal.fromInt(0).repr === "00000000")
    assert(Hexadecimal.fromInt(1).repr === "00000001")
    assert(Hexadecimal.fromInt(15).repr === "0000000F")
    assert(Hexadecimal.fromInt(30).repr === "0000001E")
  }

  test("fromInt converts negative int to hexadecimal") {
    assert(Hexadecimal.fromInt(-1).repr === "FFFFFFFF")
    assert(Hexadecimal.fromInt(-2).repr === "FFFFFFFE")
    assert(Hexadecimal.fromInt(-81).repr === "FFFFFFAF")
    assert(Hexadecimal.fromInt(-2147483648).repr === "80000000")
  }

}
