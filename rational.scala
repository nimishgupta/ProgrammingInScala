
/* n and d are class parameters
 * Scala will automatically conjure up a primary constructor that takes these two parameters from the boilerplate 
 */
class Rational (n : Int, d : Int) {
  // This shall automatically go into primary constructor body
  println ("Created " + n + "/" + d)
}
