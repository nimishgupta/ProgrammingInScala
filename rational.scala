
/* n and d are class parameters
 * Scala will automatically conjure up a primary constructor that takes these two parameters from the boilerplate 
 */
class Rational (n : Int, d : Int) {
  /* add a precondition that denominator is not 0 */
  require (d != 0)

  /* inherits the implmentation of toString defined in class java.lang.Object
   * overriding with new impl
   */
  override def toString = n + "/" + d
}
