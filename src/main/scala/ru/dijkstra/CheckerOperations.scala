package ru.dijkstra

import java.io.FileInputStream
import scalax.file.Path
import java.nio.{BufferUnderflowException, ByteBuffer}

case class Sentence(isBOM: Boolean, isValid: Boolean)

object CheckerOperations {
  def checkFile(file: Path): Sentence = {
    if (!file.exists || !file.isFile)
      throw new Exception("File reading error")
    val BUFFER_SIZE = 4096;
    val buffer = ByteBuffer.allocate(BUFFER_SIZE)
    val channel = (new FileInputStream(file.path)).getChannel()
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

  object u8Len {
    def unapply(c: Byte): Option[Int] = {
      if ((c >> 7) == 0) return Some(1)
      for (i <- 1 until 7)
        if (((c ^ (0x80 >> i)) | ((0x80 >> i) - 1)).toByte == 0xFF.toByte)
          return Some(i)
      None
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
