package tasks.adts

package u04lab

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    case class complexImpl(re: Double, im: Double)
    type Complex = complexImpl
    def complex(re: Double, im: Double): Complex =  complexImpl(re,im)
    extension (complex: Complex)
      def re(): Double = complex.re //???
      def im(): Double = complex.im //???
      def sum(other: Complex): Complex = complexImpl(re + other.re,im+other.im) //???
      def subtract(other: Complex): Complex = complexImpl(re - other.re,im - other.im) //???
      def asString(): String = re match
        case 0 => im match
          case 0 => "0.0"
          case _ =>  s"${im}i"
        case _ => im match
          case 0 => s"$re"
          case x if x < 0 => s"$re - ${im * -1}i"
          case _ => s"$re + ${im}i"
