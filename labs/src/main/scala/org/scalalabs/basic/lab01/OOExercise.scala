package org.scalalabs.basic.lab01
import scala.language.implicitConversions
/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 *
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 *
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 *   -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 *   -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method
 *
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Dollar to Euro using the
 *   [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 *
 * Exercise 5:
 * - Extend the conversion method from Dollar to Euro with an implicit parameter
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 */
class Euro(val euro: Int, val cents: Int = 0) extends Currency with Ordered[Euro] {
  def inCents = euro * 100 + cents;
  def +(parEuro: Euro): Euro = {
    Euro.fromCents(this.inCents + parEuro.inCents);
  };
  def *(factor: Int): Euro = {
    Euro.fromCents(this.inCents * factor);
  };

  override def toString: String = {
    var digitsCentsString: String = if (cents > 0 && cents < 10) '0' + cents.toString else if (cents == 0) "--" else cents.toString;
    symbol + ':' + ' ' + euro + ',' + digitsCentsString
  }

  override def compare(that: Euro): Int = inCents - that.inCents
}

object Euro {
  def fromCents(cents: Int): Euro = {
    var mod = cents % 100;
    new Euro((cents - mod) / 100, mod);
  }
  implicit class EuroInt(val factor: Int) extends AnyVal {
    def *(euro: Euro) = euro * factor // why?
  }

  // TODO: change this in solution!
  implicit def fromDollar(dollar: Dollar)(implicit converter: CurrencyConverter = DefaultCurrencyConverter): Euro = Euro.fromCents(converter.toEuroCents(dollar.inCents))
}

class Dollar(val dollar: Int, val cents: Int) extends Currency(symbol = "USD") {
  def inCents: Int = dollar * 100 + cents;
}

abstract class Currency(val symbol: String = "EUR") {
}