package ru.dijkstra

import java.io.FileInputStream
import java.nio.ByteBuffer
import scalax.file.Path

case class Sentence(isBOM: Boolean, isValid: Boolean)

object CheckerOpreations {
def checkFile(file: Path) : Sentence = {
    if (!file.exists || !file.isFile)
      throw new Exception("File reading error")
    val BUFFER_SIZE = 4096;
    val buffer = ByteBuffer.allocate(BUFFER_SIZE)
    val channel = (new FileInputStream(file.path)).getChannel()
    val size = channel.read(buffer)
    buffer.flip()
    val isBOM = if (size < 3) false
    else (List(buffer.get(), buffer.get(), buffer.get())
      == List(-17: Byte, -69: Byte, -65: Byte)) // EF BB BF
    if (!isBOM) buffer.rewind()
    var isValid = true
    try {
      var eof = false
      do {
        if (buffer.remaining() < 6 || eof) {
          buffer.compact()
          val i = channel.read(buffer)
          buffer.flip()
          eof = i == -1
        }
        check(buffer)
      } while (!eof)
    } catch {
      case ex:Exception => {
        isValid = false
        //println(ex.getMessage)
      }
    }
    Sentence(isBOM, isValid)
  }

  object u8Len_ {
    def unapply(c: Int) = {
      if (((c ^ 0x80) | 0x7f) == 0xff) Some(1)
      else if (((c ^ 0x40) | 0x3f) == 0xff) Some(2)
      else if (((c ^ 0x20) | 0x1f) == 0xff) Some(3)
      else if (((c ^ 0x10) | 0x0f) == 0xff) Some(4)
      else if (((c ^ 0x08) | 0x07) == 0xff) Some(5)
      else if (((c ^ 0x04) | 0x03) == 0xff) Some(6)
      else throw new Exception("u8Len FAIL")
    }
  }

  object u8Len {
    def unapply(e: Int) = {
      val c = e & 0xff
      if ((c >> 7) == 0) Some(1)
      else if ((c >> 5) == 6 /*110b*/) Some(2)
      else if ((c >> 4) == 14 /*1110b*/) Some(3)
      else if ((c >> 3) == 30 /*11110b*/) Some(4)
      else if ((c >> 2) == 62 /*111110b*/) Some(5)
      else if ((c >> 1) == 126 /*1111110b*/) Some(6)
      else if ((c >> 6) == 3 /*10b*/) None
      else None

    }
  }

  def check(in: ByteBuffer) {
    val b = in.get()
    u8Len.unapply(b) match {
      case Some(l) => checkData(l - 1, in)
      case None => throw new Exception("Check FAIL")
    }

  }

  def checkData(length: Int, in: ByteBuffer) {
    for (i <- 0 until length) {
      val c = in.get()
      //println(Integer.toBinaryString(c & 0xFF))
      if (((c & 0xFF) >> 6) != 2 /*10b*/)
        throw new Exception("checkData fail @ " + i)
    }
  }
}