package ru.dijkstra

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class Scripttest extends FreeSpec with ShouldMatchers {
  "u8Len test" - {
    import CheckerOpreations.u8Len
    def getFirsByte(s: String) : Int = {
      import scala.io.Codec
      val a = Codec.toUTF8(s)
     // println(Integer.toBinaryString(a.head & 0xff))
     // println(u8Len.unapply(a.head & 0xff))
      a.head & 0xff
    }
    "  u8len '0'" in {
      u8Len.unapply(getFirsByte("0")) should be === Some(1)
    }
    "  u8len 'х'" in {
      u8Len.unapply(getFirsByte("х")) should be === Some(2)
    }
    "  u8len 'て'" in {
      u8Len.unapply(getFirsByte("て")) should be === Some(3)
    }
    /*
    "  u8len '裔'" in {
      u8Len.unapply(getFirsByte("裔")) should be === Some(4)
    }                                                         */
  }
}
