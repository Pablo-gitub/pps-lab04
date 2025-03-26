package tasks.adts

import java.util.Locale

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
    case class ComplexImpl(re: Double, im: Double)
    // Change assignment below: should probably define a case class and use it?
    type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = ComplexImpl(complex.re + other.re, complex.im + other.im)
      def subtract(other: Complex): Complex = ComplexImpl(complex.re - other.re, complex.im - other.im)
      def asString(): String = complex match
        case ComplexImpl(_, 0) => String.format(Locale.US, "%.1f", java.lang.Double.valueOf(complex.re))
        case ComplexImpl(0, _) => String.format(Locale.US, "%.1fi", java.lang.Double.valueOf(complex.im))
        case ComplexImpl(re, im) if im < 0  => String.format(Locale.US, "%.1f - %.1fi", java.lang.Double.valueOf(complex.re), math.abs(java.lang.Double.valueOf(complex.im)))
        case _ => String.format(Locale.US, "%.1f + %.1fi", java.lang.Double.valueOf(complex.re), java.lang.Double.valueOf(complex.im))