package learning.scala

import org.scalatest.FunSuite

class Chapter5Spec extends FunSuite with Chapter5 {

  test("function literal that takes two integers and returns the higher number") {
    assert(max(4, 2) == 4)
    assert(max(1, 3) == 3)
  }

  test("a higher-order function that takes a 3-sized tuple of integers plus " +
    "this function literal, and uses it to return the maximum value in the tuple") {
    assert(max3((1, 3, 5))(max) == 5)
    assert(max3((7, 3, 5))(max) == 7)
    assert(max3((1, 9, 5))(max) == 9)
    assert(max3((9, 9, 5))(max) == 9)
    assert(max3((9, 9, 9))(max) == 9)
    assert(max3((4, 9, 9))(max) == 9)
  }

}
