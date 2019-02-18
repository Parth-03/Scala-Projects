class Tests extends org.scalatest.FunSuite {

  import Main._
import hw.streams.Generator._

  test("Implemented the Solution interface") {
    val main: hw.streams.SolutionLike = Main
  }

  test("interleave")
  {
    def isEven(x: Int): Boolean = x%2 == 0
    def isOdd(x: Int): Boolean = x%2!=0

    val first = filter(isEven, from(0))
    val second = filter(isOdd, from(0))

    assert(nth(interleave(first, second), 0) == 0)
    assert(nth(interleave(first, second), 1) == 1)
    assert(nth(interleave(first, second), 2) == 2)
    assert(nth(interleave(first, second), 3) == 3)
    assert(nth(interleave(first, second), 4) == 4)
    assert(nth(interleave(first, second), 100) == 100)
  }

  test("sift")
  {
    assert(nth(sift(4,pow), 0) ==1)
    assert(nth(sift(4,pow), 1) ==2)
    assert(nth(sift(4,pow), 2) !=4)
    assert(nth(sift(4,pow), 3) !=8)
    assert(nth(sift(4,pow), 4) !=16)
  }

 
}
