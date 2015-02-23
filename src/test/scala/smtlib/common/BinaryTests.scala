package smtlib
package common

import org.scalatest.FunSuite

class BinaryTests extends FunSuite {

  test("toIntBits works with one bit") {
    assert(Binary(List(true)).toIntBits === 1)
    assert(Binary(List(false)).toIntBits === 0)
  }

  test("toIntBits works with a few bits") {
    assert(Binary(List(true, true)).toIntBits === 3)
    assert(Binary(List(true, false, true)).toIntBits === 5)
  }

  test("toIntBits correctly ignores leading zeros") {
    assert(Binary(List(false, true)).toIntBits === 1)
    assert(Binary(List(false, true, false, true)).toIntBits === 5)
  }

  test("toLongBits works with one bit") {
    assert(Binary(List(true)).toLongBits === 1)
    assert(Binary(List(false)).toLongBits === 0)
  }

  test("toLongBits works with a few bits") {
    assert(Binary(List(true, true)).toLongBits === 3)
    assert(Binary(List(true, false, true)).toLongBits === 5)
  }

  test("toLongBits correctly ignores leading zeros") {
    assert(Binary(List(false, true)).toLongBits === 1)
    assert(Binary(List(false, true, false, true)).toLongBits === 5)
  }

  test("toInt works with one bit") {
    assert(Binary(List(true)).toInt === -1)
    assert(Binary(List(false)).toInt === 0)
  }

  test("toInt works with a few bits") {
    assert(Binary(List(true, true)).toInt === -1)
    assert(Binary(List(false, true, true)).toInt === 3)
    assert(Binary(List(false, true, false, true)).toInt === 5)
    assert(Binary(List(true, false, true)).toInt === -3)
  }

}
