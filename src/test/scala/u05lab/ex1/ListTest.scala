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
    val even = 10 :: 30 :: Nil()
    val odd = 21 :: 41 :: Nil()
    assertEquals((even, odd), l.partition(_ % 2 == 0))

  @Test
  def testSpan(): Unit =
    val l = 10 :: 20 :: 31 :: 40 :: Nil()
    val l1 = 10 :: 20 :: Nil()
    val l2 = 31 :: 40 :: Nil()
    assertEquals((l1, l2), l.span(_ % 2 == 0))

  @Test
  def testReduce(): Unit =
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals(100, l.reduce(_ + _))

  @Test
  def testTakeRight(): Unit =
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    val l1 = 30 :: 40 :: Nil()
    assertEquals(l1, l.takeRight(2))
}
