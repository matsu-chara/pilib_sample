import scala.concurrent.pilib._

// from: [PiLib: A Hosted Language for Pi-Calculus Style Concurrency - Vincent Cremet, Martin Odersky](http://lampwww.epfl.ch/~cremet/publications/pilib.pdf)
object Buffer {

  /**
   * Chan[A]と型チェックが効くのがPiLibの利点
   *
   * Buffer(put, get) = B0(put, get)
   * B0(put, get) = put(x).B1(put, get, x)
   * B1(put, get, x) = get$<x>.B0(put, get) + put(y).B2(put, get, x, y)
   * B2(put, get, x, y) = get$<x>.B1(put, get, y)
   */
  def Buffer[A](put: Chan[A], get: Chan[A]): Unit = {
    def B0: Unit = choice(put * { x => println("B0 => B1"); B1(x) })
    def B1(x: A): Unit = choice(get(x) * {
      println("B1 => B0"); B0
    }, put * { y => println("B1 => B2"); B2(x, y) })
    def B2(x: A, y: A): Unit = choice(get(x) * {
      println("B2 => B1"); B1(y)
    })
    B0
  }

  // A π-calculus channel is a channel that can carry other π-calculus channel
  class Channel extends Chan[Channel]

  /**
   * Producer(put, get) = νx.put$<x>.Producer(put, get)
   * Consumer(put, get) = get(x).Consumer(put, get)
   *
   * ν put, get. Producer(put, get) | Buffer(put, get) | Consumer(put, get)
   */
  def Producer(put: Channel, get: Channel, count: Int = 5): Unit = {
    if (count > 0) {
      val x = new Channel
      choice(put(x) * {
        println("current: " + count)
        Producer(put, get, count - 1)
      })
    } else {
      sys.exit(0)
    }
  }

  def Consumer(put: Channel, get: Channel): Unit = {
    choice(get * { x =>
      Consumer(put, get)
    })
  }

  def main(args: Array[String]): Unit = {
    val put = new Channel
    val get = new Channel
    spawn < Producer(put, get) | Buffer(put, get) | Consumer(put, get) >
  }
}
