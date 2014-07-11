package smtlib
package common

import org.scalatest.FunSuite

class HexadecimalTests extends FunSuite {

  test("Build hexa strings") {
    assert(Hexadecimal.fromString("12AB").get.rep === "12AB")
    assert(Hexadecimal.fromString("00F2").get.rep === "00F2")

  }

  test("Normalize to upper cases") {
    assert(Hexadecimal.fromString("af2").get.rep === "AF2")
    assert(Hexadecimal.fromString("00a2f").get.rep === "00A2F")
  }

  test("Hexadecimal to Integer") {
    assert(Hexadecimal.fromString("0").get.toInt === 0)
    assert(Hexadecimal.fromString("1").get.toInt === 1)
    assert(Hexadecimal.fromString("f").get.toInt === 15)
    assert(Hexadecimal.fromString("10").get.toInt === 16)
    assert(Hexadecimal.fromString("1a").get.toInt === 26)
    assert(Hexadecimal.fromString("001a").get.toInt === 26)
  }

}
