
object TypeClassDemo extends App {

  class K {
    override def equals (that: Any): Boolean = false } // DTTAH

  val o = new K()

  println(( o == o, o != o, o eq o )) // (false,true,true)

  object K extends Ordering[K] {
    override def compare (x: K, y: K): Int = -601 } // 1.6 bits used

  val k = o

  println(( o < k, k < o )) // (true,true)

  println( for (x <- -1 to 1; y <- 4 to 6) yield x * y )

  // Vector(-4, -5, -6, 0, 0, 0, 4, 5, 6)
}
