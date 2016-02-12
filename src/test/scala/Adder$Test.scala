/**
  * Created by poxenha on 12/02/2016.
  */
class Adder$Test extends org.scalatest.FunSuite {

  test("Adding two numbers") {
    assert(Adder.addition(1,1) == 2)
  }

  test("Adding two large numbers") {
    assert(Adder.addition(1000000, 2121000) == 1000000 + 2121000)
  }

  test("Adding with negative") {
    assert(Adder.addition(1000000, -2121) == 1000000 - 2121)
  }

  test("Adding with multiple negative") {
    assert(Adder.addition(1000000, -2121, -30) == 1000000 + -2121 + -30)
  }

  test("Adding with both negative") {
    assert(Adder.addition(-1000000, -2121) == -1000000 - 2121)
  }

  test("Adding zero") {
    assert(Adder.addition(0, 0) == 0)
  }

  test("Subtracting two numbers") {
    assert(Adder.subtraction(1, 1) == 1 - 1)
  }

  test("Subtracting two large numbers") {
    assert(Adder.subtraction(1000000, 2121000) == 1000000 - 2121000)
  }

  test("Subtracting with negative") {
    assert(Adder.subtraction(1000000, -2121) == 1000000 - -2121)
  }

  test("Subtracting with both negative") {
    assert(Adder.subtraction(-1000000, -2121) == -1000000 - -2121)
  }

  test("Subtracting zero") {
    assert(Adder.subtraction(0, 0) == 0)
  }

}
