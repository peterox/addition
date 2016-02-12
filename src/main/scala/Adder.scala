
/**
  * Created by poxenha on 12/02/2016.
  */

object Adder {

  def main(args: Array[String]) {

    args.toList match {
      case "+"::next::rest => println(addition((next::rest).map(_.toLong):_*))
      case "-"::next::rest => println(subtraction((next::rest).map(_.toLong):_*))
      case _ => {
        Console.err.println("Usage: +|- num [num]")
        System.exit(1)
      }
    }
    System.exit(0)
  }

  def addition(a:Long*) = {
    a.reduce(binary_addition)
  }

  def subtraction(a:Long*) = {
    a.reduce(binary_subtraction)
  }

  private def binary_subtraction(a:Long, b:Long) = {
    addition(a, b * -1)
  }

  private def binary_addition(a:Long, b:Long) = {
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

}


