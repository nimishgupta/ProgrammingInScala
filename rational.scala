
/* n and d are class parameters
 */
class Rational (n : Int, d : Int) {
  /* Any free code goes into the primary constructor */

  /* add a precondition that denominator is not 0 */
  require (d != 0)

  /* Explicit fields, required to access similar fields of objects
   * other than "this"
   */
  val numer = n
  val denom = d

  /* auxiliary constructor */
  def this (n : Int) = this (n, 1)

  /* inherits the implmentation of toString defined in class java.lang.Object
   * overriding with new impl
   */
  override def toString = n + "/" + d

  def add (that: Rational) : Rational = 
    new Rational (numer * that.denom + that.numer * denom, denom * that.denom)

  def lessthan (that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  def max (that: Rational) =
    if (this lessthan that) that else this
}
