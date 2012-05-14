package ru.dijkstra

import java.io.FileInputStream
import scalax.file.Path
import java.nio.{BufferUnderflowException, ByteBuffer}
import annotation.tailrec

case class Sentence(isBOM: Boolean, isValid: Boolean)

object CheckerOperations {
  import resource._
  private val BUFFER_SIZE = 4096;
  val buffer = ByteBuffer.allocate(BUFFER_SIZE)

  def checkFile(file: Path): Sentence = {
    if (!file.exists || !file.isFile) {
      throw new Exception("File reading error")
    }
    val a = for (stream <- managed(new FileInputStream(file.path));
         channel <- managed(stream.getChannel)) yield {
      val size = channel.read(buffer)
      buffer.flip()
      val isBOM = if (size < 3) false
      else (List(buffer.get(), buffer.get(), buffer.get())
        == List(0xEF.toByte, 0xBB.toByte, 0xBF.toByte))
      if (!isBOM) buffer.rewind()
      var isValid = true
      var eof = false
      do {
        if (buffer.remaining() < 6 || eof) {
          buffer.compact()
          val i = channel.read(buffer)
          buffer.flip()
          eof = i == -1
        }
        try {
          check(buffer) match {
            case Some(_) => isValid = false
            case None =>
          }
        } catch {
          case e: BufferUnderflowException => isValid = false
        }
      } while (!eof && isValid)
      Sentence(isBOM, isValid)
    }
    a.reflect
  }

  object u8Len {
    private val marker = 0x80
    private val full = 0xff
    private val fullB = full.toByte

    def unapply(c: Byte): Option[Int] = {
      @tailrec def rec(in: Byte, shift: Int): Option[Int] = {
        if (shift >= 7) {
          None
        } else {
          val m = (marker >>> shift).toByte
          val rest = (full >>> (shift + 1)).toByte
          val total = (in ^ m) | rest
          if (total == fullB) Some(1 max shift) else rec(in, shift + 1)
        }
      }
      rec(c, 0)
    }
  }

  def check(in: ByteBuffer): Option[Error] = {
    u8Len.unapply(in.get()) match {
      case Some(l) => for (i <- 0 until l - 1)
        if (((in.get() & 0xFF) >> 6) != 2 /*10b*/ )
          return Some(new Error("Tailing byte has head-alike value"))
      case None => return Some(new Error("Cannot decipher head byte"))
    }
    None
  }
}
