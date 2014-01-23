/* composition and inheritance */


import Element.elem


/* Abstract class, instantiating it is an error */
abstract class Element {

  /* Parameterless methods, convention to signify no side effects. 
   * It does not matter if its a method or field
   */
  def contents: Array[String] /* (implicitly) Abstract method */
  def height: Int = contents.length
  def width: Int = if (contents.length == 0) 0 else contents(0).length

  def above (that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem (this1.contents ++ that1.contents)
  }

  def beside (that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem (for ((line1, line2) <- this1.contents zip that1.contents) yield line1 + line2)
  }

  /* Widen the element to specific width */
  def widen (w: Int): Element =
    if (w <= width) this
    else {
      val left  = elem (' ', (w - width)/2, height)
      val right = elem (' ', w - width - left.width, height)
      left beside this beside right
    }

  def heighten (h: Int): Element =
    if (h <= height) this
    else {
      val top = elem (' ', width, (h - height)/2)
      var bot = elem (' ', width, h - height - top.height)
      top above this above bot
    }

  override def toString = contents mkString "\n"
}

/* Object factory with the help of companion object */
object Element {

  /* Concrete class of abstract class Element
   *
   * Private members are not inherited
   * Those members of superclass are not inherited when subclass contains
   * members with same name as that of superclass. They are instead said
   * to be "overridden/implemented" by subclasss.
   */

  /* contents is a field of ArrayElement */
  class ArrayElement(val contents: Array[String]) extends Element {

    /* Overriding contents of superclass
     *
     * This is currently a (paramterless) method but it could have been
     * easily defined as a field. In scala, a field is allowed to override
     * a method and vice versa.
     *
     * def contents: Array[String] = conts
     */
  }


  /* Example of calling a super class constructor
   *
   * "override"ing is required when we are reimplementing a concrete property
   * and it is optional when implementing an abstract method
   * class LineElement (s: String) extends ArrayElement (Array(s) {
   * override def width = s.length
   * override def height = 1
   }
   */

  /*
   * "override"ing is required when we are reimplementing a concrete property
   * and it is optional when implementing an abstract method
   */
  class LineElement (s: String) extends Element {
    val contents = Array(s)
    override def width = s.length
    override def height = 1
  }


  class UniformElement(ch: Char,
    override val width: Int,
    override val height: Int) extends Element {
    private val line = ch.toString * width
      def contents = Array.fill (height)(line)
  }


  def elem (contents: Array[String]): Element =
    new ArrayElement (contents)

  def elem (chr: Char, width: Int, height: Int): Element =
    new UniformElement (chr, width, height)

  def elem (line: String): Element =
    new LineElement(line)
}



object Spiral {

  val space = elem (" ")
  val corner = elem ("+")

  def spiral (nEdges: Int, direction: Int): Element = {
    if (nEdges == 1) elem ("+")
    else {
      val sp = spiral (nEdges - 1, (direction + 3)%4)
      def verticalBar = elem ('|', 1, sp.height)
      def horizontalBar = elem ('-', sp.width, 1)
      if (direction == 0)
        (corner beside horizontalBar) above (sp beside space)
      else if (direction == 1)
        (sp above space) beside (corner above verticalBar)
      else if (direction == 2)
        (space beside sp) above (horizontalBar beside corner)
      else
        (verticalBar above corner) beside (space above sp)
    }
  }

  def main (args: Array[String]) {
    val nSides = args (0).toInt
    println (spiral(nSides, 0))
  }
}
