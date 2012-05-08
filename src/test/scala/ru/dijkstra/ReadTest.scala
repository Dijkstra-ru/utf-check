package ru.dijkstra

import org.scalatest.FreeSpec
import scalax.file.Path
import java.io.FileInputStream
import resource._
import java.nio.ByteBuffer
import org.scalatest.matchers.{MatchResult, BeMatcher, ShouldMatchers}
import java.util.Arrays

/**
 * @author eiennohito
 * @since 08.05.12
 */

class ReadTest extends FreeSpec with ShouldMatchers {

  def data(in: Array[Byte]) = new BeMatcher[ByteBuffer] {
    def apply(left: ByteBuffer) = {
      val d = new Array[Byte](in.length)
      left.flip()
      left.get(d)
      MatchResult(
        Arrays.equals(d, in),
        "Data in byte buffer were not eqial, should be [%s], was [%s]".format(in.mkString(", "), d.mkString(", ")),
        "Data in byte buffer were equal"
      )
    }
  }

  "File Test" - {
    "can read buffer twice" in {
      val p = Path("src/test/resources/test_file")
      val buf1 = ByteBuffer.allocate(8)
      val buf2 = ByteBuffer.allocate(8)
      for(fs <- managed(new FileInputStream(p.toAbsolute.path));
          chn <- managed(fs.getChannel)) {
        val i1 = chn.read(buf1)
        i1 should be (8)
        buf1 should be (data(Array[Byte]('i', ' ', 'a', 'm'))) //should read 4 bytes from first buffer
        buf1.compact()
        val i2 = chn.read(buf1)
        i2 should be (4)
        buf1 should be (data(Array[Byte](' ', 't', 'e', 's'))) //should read 4 bytes from first buffer
        buf1.compact()
        val i3 = chn.read(buf1)
        i3 should be (4)
        buf1 should be (data(Array[Byte]('t', ' ', 'f', 'i', 'l', 'e')))
        buf1.rewind()
        val i4 = chn.read(buf1)
        i4 should be (-1)
      }
    }
  }
}
