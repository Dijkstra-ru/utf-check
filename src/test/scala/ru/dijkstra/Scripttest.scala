package ru.dijkstra

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers

class Scripttest extends FreeSpec with ShouldMatchers {
  "u8Len test" - {
    import CheckerOperations.u8Len
    def getFirsByte(s: String) : Byte = {
      import scala.io.Codec
      Codec.toUTF8(s) head
     // println(Integer.toBinaryString(a.head & 0xff))
     // println(u8Len.unapply(a.head & 0xff))
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
    "  u8len 4" in {
      u8Len.unapply(0xF0.toByte) should be === Some(4)
    }
    "  u8len 5" in {
      u8Len.unapply(0xF8.toByte) should be === Some(5)
    }
    "  u8len 6" in {
      u8Len.unapply(0xFC.toByte) should be === Some(6)
    }
  }
}
