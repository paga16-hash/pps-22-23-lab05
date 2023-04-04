package u05lab.ex1

import org.junit.Assert.assertEquals
import org.junit.Test

class ListTest {
  import u05lab.ex1.List.*

  /** todo */
  @Test
  def testZipRight(): Unit =
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals((10, 0) :: (20, 1) :: (30, 2) :: (40, 3) :: Nil(), l.zipRight)

  @Test
  def testPartition(): Unit =
    val l = 10 :: 21 :: 30 :: 41 :: Nil()
    assertEquals((10 :: 30 :: Nil(), 21 :: 41 :: Nil()), l.partition(_ % 2 == 0))
}
