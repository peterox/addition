
/**
  * Created by poxenha on 12/02/2016.
  */

object Adder {
  def subtraction(a:Long, b:Long) = {
    addition(a, b * -1)
  }

  def binary_addition(a:Long, b:Long) = {
    def _inner(a:Char, b:Char, carryIn:Char):(Char,Char) =  {
      (a, b, carryIn) match {
        case ('1', '1', '1') => ('1', '1')
        case ('1', '1', '0') => ('0', '1')
        case ('1', '0', '1') => ('0', '1')
        case ('1', '0', '0') => ('1', '0')
        case ('0', '1', '1') => ('0', '1')
        case ('0', '1', '0') => ('1', '0')
        case ('0', '0', '1') => ('1', '0')
        case ('0', '0', '0') => ('0', '0')
      }
    }

    val zipped = a.toBinaryString.reverse.zipAll(b.toBinaryString.reverse, '0', '0')

    val (result, carryOut) = zipped.foldLeft((List[Char](), '0'))((resultWithCarryIn, input) => {
      val (result, carryIn) = resultWithCarryIn
      val (a, b) = input
      val (comp, carryOut) = _inner(a, b, carryIn)
      (result :+ comp, carryOut)
    })
    BigInt((if (b > 0) result :+ carryOut else result).reverse.mkString, 2).toLong
  }

  def addition(a:Long*) = {
    a.reduce(binary_addition)
  }
}


