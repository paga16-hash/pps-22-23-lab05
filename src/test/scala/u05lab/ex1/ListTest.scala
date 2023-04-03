package u05lab.ex1

import org.junit.Assert.assertEquals
import org.junit.Test

class ListTest {
  import u05lab.ex1.List.*

  /** todo */
  @Test
  def testZipRight(): Unit =
    val l = 10 :: 20 :: 30 :: Nil()
    assertEquals((10, 0) :: (20, 0) :: (30, 0) :: Nil(), l.zipRight)
    //assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))
}
