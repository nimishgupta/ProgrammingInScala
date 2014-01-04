/* everything is a method in scala */

val a = 4

/* to is a function called on 'a' with 10 as paramter
 * the below code is equivalent to a.to (10)
 */
for (i <- a to 10) Console print i


val arr = new Array[String](3)
arr(0) = "Hello" /* == arr.update (0, Hello)
