
/* n and d are class parameters
 */
class Rational (n : Int, d : Int) {
  /* Any free code goes into the primary constructor */

  /* add a precondition that denominator is not 0 */
  require (d != 0)

  private val g = gcd (n.abs, d.abs)

  /* No type inference on return type due to recursive method, need to provide
   * it explicitly
   */
  private def gcd (n : Int, d : Int) : Int =
    if (d == 0) n else gcd (d, n % d)

  /* Explicit fields, required to access similar fields of objects
   * other than "this"
   */
  val numer = n/g
  val denom = d/g

  /* auxiliary constructor */
  def this (n : Int) = this (n, 1)

  /* inherits the implmentation of toString defined in class java.lang.Object
   * overriding with new impl
   */
  override def toString = numer + "/" + denom

  /////////////// ADD /////////////////////////
  def + (that: Rational) : Rational = 
    new Rational (numer * that.denom + that.numer * denom, denom * that.denom)

  /* operator overloading, return type required */
  def + (numer: Int) : Rational =
    this + new Rational (numer)



  //////////////// Subtract //////////////////////
  def - (that: Rational) =
    new Rational (numer * that.denom - that.numer * denom, denom * that.denom)

  /* operator overloading, return type required */
  def - (numer: Int) : Rational =
    this - new Rational (numer)



  ////////////////// Multiply //////////////////
  def * (that: Rational) =
    new Rational (numer * that.numer, denom * that.denom)

  /* Operator overloading */
  def * (numer: Int) =
    /* this * new Rational (numer) */
    new Rational (this.numer * numer, this.denom);



  /////////////////// Divide //////////////////
  def / (that: Rational) =
    this * new Rational (that.denom, that.numer)

  /* operator overloading, return type required */
  def / (numer: Int) : Rational =
    this * new Rational (1, numer)


  ///////////////// Comparator ////////////////////
  def < (that: Rational) =
    this.numer * that.denom < this.denom * that.numer

  /* Operator overloading, need a return type */
  def < (numer: Int): Boolean =
    this < new Rational (numer)



  def max (that: Rational) =
    if (this < that) that else this
}
