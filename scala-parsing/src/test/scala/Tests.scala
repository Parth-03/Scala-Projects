import hw.parsing._
class TrivialTestSuite extends org.scalatest.FunSuite {

  test("several objects must be defined") 
  {
    val parser: hw.parsing.ArithParserLike = ArithParser
    val printer: hw.parsing.ArithPrinterLike = ArithPrinter
    val eval: hw.parsing.ArithEvalLike = ArithEval

    val test1 = ArithParser.parseArith("1")
    val test2 = ArithParser.parseArith("1+2")
    val test3 = ArithParser.parseArith("2*3+5*-10")
    val test4 = ArithParser.parseArith("2*(3+5)*-10")
    val test5 = ArithParser.parseArith("2*(3+5)^2*-10") 
    
    assert(test1 == Num(1))
    assert(test2 == Add(Num(1),Num(2)))
    assert(test3 == Add(Mul(Num(2.0),Num(3.0)),Mul(Num(5.0),Num(-10.0))))
    assert(test4 == Mul(Mul(Num(2.0),Add(Num(3.0),Num(5.0))),Num(-10.0)))
    assert(test5 == Mul(Mul(Num(2.0),Exponent(Add(Num(3.0),Num(5.0)),Num(2.0))),Num(-10.0)))

    assert(ArithEval.eval(test3) == -44)
    assert(ArithEval.eval(test4) == -160)
    assert(ArithEval.eval(test5) == -1280)
    
    assert(ArithPrinter.print(test3) == "((2.0*3.0)+(5.0*-10.0))")
    assert(ArithPrinter.print(test4) == "((2.0*(3.0+5.0))*-10.0)")
    assert(ArithPrinter.print(test5) == "((2.0*((3.0+5.0)^2.0))*-10.0)")

  }

  
}